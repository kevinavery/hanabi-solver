package io.avery.hanabi

import scala.annotation.tailrec
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random

case class ActionTriple(pGiveInformation: Double, pDiscardCard: Double, pPlayCard: Double)

object HanabiSolver extends App {
  val NumTrials = 500
  val TrialDepthMax = 15

  def allHands(handSize: Int, cards: Vector[KnownCard]): Stream[Vector[KnownCard]] = {
    def genHand(partialHand: Vector[KnownCard], remaining: Vector[KnownCard]): Stream[Vector[KnownCard]] = {
      if (partialHand.size == handSize) {
        Stream(partialHand)
      } else {
        remaining.toStream.zipWithIndex.flatMap(e => genHand(partialHand :+ e._1, remaining.patch(e._2, Vector(), 1)))
      }
    }
    genHand(Vector(), cards)
  }

  // For the given (unordered) set of cards that could make up a hand, and ordered constraints on the hand,
  // find the ordered set of cards that satisfy the constraints.
  def checkHandConstraints(handGuess: Vector[KnownCard], constraints: Vector[UnknownCard]): Option[Vector[KnownCard]] = {
    handGuess.permutations.find(perm =>
      perm.zip(constraints).forall {
        case ((card, unknownCard)) => unknownCard.info.forall {
          info => info match {
            case KnownNumber(n) => card.number == n
            case KnownNotNumber(n) => card.number != n
            case KnownColor(c) => card.color == c
            case KnownNotColor(c) => card.color != c
          }
        }
      }
    )
  }

  def guessHand(player: Player, game: Game): Vector[KnownCard] = {
    val unseenCards = Random.shuffle(game.deck.cards.toVector ++ player.knownHand)
    allHands(player.unknownHand.size, unseenCards).flatMap(checkHandConstraints(_, player.unknownHand)).head
  }

  def computeCardCertainty(unknownCard: UnknownCard): Double = {
    val weights = unknownCard.info.map(
      info => info match {
        case _: KnownNumber => 0.5
        case _: KnownColor => 0.5
        case _: KnownNotNumber => 0.1
        case _: KnownNotColor => 0.1
      }
    )
    weights.sum.min(1.0)
  }

  def computeHandCertainty(handInfo: Vector[UnknownCard]): Vector[Double] = {
    handInfo.map(computeCardCertainty)
  }

  def playCardScores(player: Player, game: Game): Vector[Int] = {
    player.knownHand.map { c =>
      if (game.isPlayLegal(c)) {
        if (c.number == 5) {
          2
        } else {
          1
        }
      } else {
        0
      }
    }
  }

  def discardCardScores(player: Player, game: Game): Vector[Int] = {
    player.knownHand.map { c =>
      val goodDiscard = game.fireworks.get(c.color).flatMap(_.topCardMaybe.map(_.number)).getOrElse(0) >= c.number
      val liveCards = game.deck.cards ++ game.players.filterNot(_.id == game.currentPlayer.id).map(_.knownHand)
      val unsafeDiscard = !goodDiscard || !liveCards.contains(c)
      if (goodDiscard) {
        2
      } else if (unsafeDiscard) {
        0
      } else {
        1
      }
    }
  }

  def computeProbabilityPlayCardGood(player: Player, game: Game): Vector[Double] = {
    playCardScores(player, game).zipWithIndex.map {
      case ((score, index)) if score > 0 => computeCardCertainty(player.unknownHand(index))
      case _ => 0.0
    }
  }

  def computeProbabilityDiscardCardGood(player: Player, game: Game): Vector[Double] = {
    discardCardScores(player, game).zipWithIndex.map {
      case ((score, index)) if score > 0 => computeCardCertainty(player.unknownHand(index))
      case _ => 0.0
    }
  }

  // The game strategy is coded by defining an ActionTriple for various situations
  // in the game. I've just guessed at the values, so tuning here could give much
  // better performance.
  def computeActionProbabilities(game: Game): ActionTriple = {
    val pPlayCardGood = computeProbabilityPlayCardGood(game.currentPlayer, game).max
    val pDiscardCardGood = computeProbabilityDiscardCardGood(game.currentPlayer, game).max
    if (game.lastRound) {
      if (game.players.filter(p => p != game.currentPlayer && !p.lastTurn).size == 0) {
        ActionTriple(0, .5, .5)
      } else {
        if (pDiscardCardGood < .5 && pPlayCardGood < .5) {
          ActionTriple(0, .5, .5)
        } else {
          ActionTriple(0, .3, .7)
        }
      }
    } else {
      if (game.numFuseTokens == 1) {
        if (game.numInfoTokens == 0) {
          ActionTriple(0, .95, .05)
        } else {
          if (pPlayCardGood < .9) {
            ActionTriple(.59, .4, .01)
          } else {
            ActionTriple(.05, .05, .9)
          }
        }
      } else {
        if (game.numInfoTokens == 0) {
          if (pDiscardCardGood < .9) {
            if (pPlayCardGood < .9) {
              ActionTriple(0, .8, .2)
            } else {
              ActionTriple(0, .1, .9)
            }
          } else {
            if (pPlayCardGood < .9) {
              ActionTriple(0, .99, .01)
            } else {
              ActionTriple(0, .3, .7)
            }
          }
        } else {
          if (pDiscardCardGood < .9) {
            if (pPlayCardGood < .9) {
              ActionTriple(.8, .15, .05)
            } else {
              ActionTriple(.25, .05, .7)
            }
          } else {
            if (pPlayCardGood < .9) {
              ActionTriple(.15, .8, .05)
            } else {
              ActionTriple(.1, .45, .45)
            }
          }
        }
      }
    }
  }

  // Given some positive information, returns the vector of informations (positive or negative) for the hand.
  // Cannot be used for current player (don't know hand).
  def buildNewInformationVector(player: Player, newInfo: PositiveInformation): Vector[Information] = {
    player.knownHand.zip(player.unknownHand).map {
      case ((known, unknown)) => newInfo match {
        case KnownNumber(n) if (known.number == n) => KnownNumber(n)
        case KnownNumber(n) if (known.number != n) => KnownNotNumber(n)
        case KnownColor(c) if (known.color == c) => KnownColor(c)
        case KnownColor(c) if (known.color != c) => KnownNotColor(c)
      }
    }
  }

  def combineInformation(unknownHand: Vector[UnknownCard], newHandInfo: Vector[Information]): Vector[UnknownCard] = {
    newHandInfo.zipWithIndex.map {
      case ((info, cardIndex)) => {
        // Add the new info, but filter down to the minimum set of information.
        val newInfo = unknownHand(cardIndex).info + info
        val filteredInfo = newInfo.filter(i => i match {
          case _: KnownNotNumber => !newInfo.exists(ki => ki match {
            case _: KnownNumber => true
            case _ => false
          })
          case _: KnownNotColor => !newInfo.exists(ki => ki match {
            case _: KnownColor => true
            case _ => false
          })
          case _ => true
        })
        UnknownCard(filteredInfo)
      }
    }
  }

  // Return the positive information that maximally increases the player's hand certainty, which
  // can be used to build the new information vector for the player's hand.
  def maximizeHandCertainty(player: Player): PositiveInformation = {
    val infos = List(1, 2, 3, 4, 5).map(KnownNumber(_)) ++ Colors.AllColors.map(KnownColor(_))
    infos.map(i => (i, computeHandCertainty(combineInformation(player.unknownHand, buildNewInformationVector(player, i))).sum)).maxBy(_._2)._1
  }

  def buildGiveInformation(game: Game): GiveInformation = {
    // Who has the greatest disparity between ability to do good and confidence?
    val recipient = game.players.filter(p => p != game.currentPlayer && !p.lastTurn).map { p =>
      val amountOfGood = (computeProbabilityPlayCardGood(p, game).sum + computeProbabilityDiscardCardGood(p, game).sum)
      val confidence = computeHandCertainty(p.unknownHand).sum.max(.01)
      (p, amountOfGood / confidence)
    }.maxBy(e => e._2)._1

    GiveInformation(game.currentPlayer, recipient, buildNewInformationVector(recipient, maximizeHandCertainty(recipient)))
  }

  def buildDiscardCard(game: Game): DiscardCard = {
    val scores = discardCardScores(game.currentPlayer, game)
    val index = scores.indexOf(scores.max)
    DiscardCard(game.currentPlayer, index)
  }

  def buildPlayCard(game: Game): PlayCard = {
    val scores = playCardScores(game.currentPlayer, game)
    val index = scores.indexOf(scores.max)
    PlayCard(game.currentPlayer, index)
  }

  def pickAction(game: Game): Action = {
    val p = computeActionProbabilities(game)
    val r = Random.nextDouble()
    if (r < p.pGiveInformation) {
      buildGiveInformation(game)
    } else if (r < p.pGiveInformation + p.pDiscardCard) {
      buildDiscardCard(game)
    } else {
      buildPlayCard(game)
    }
  }

  def doTrial(game: Game): (Int, Action) = {
    val playerId = game.currentPlayer.id

    def guess(game: Game): (Action, Game) = {
      // Make a guess for our current hand, and swap it into the game.
      // TODO: When other players guess what their hand + deck should be, it should not
      // persist past their turn.
      val cp = game.currentPlayer
      val handGuess = guessHand(cp, game)
      val deckGuess = game.deck.queueCards(cp.knownHand).removeCards(handGuess)
      val playerGuess = cp.copy(knownHand = handGuess)
      val gameGuess = game.copy(deck = deckGuess, players = game.players.updated(game.turn, playerGuess))
      val action = pickAction(gameGuess)
      (action, gameGuess)
    }

    @tailrec
    def play(game: Game, curDepth: Int): Game = {
      if (game.isOver || curDepth >= TrialDepthMax) {
        game
      } else {
        val (action, newGame) = guess(game)
        print(s"$curDepth\r")
        play(applyAction(action, newGame), curDepth + 1)
      }
    }

    val (firstAction, gameGuess) = guess(game)
    val finalGameGuess = play(applyAction(firstAction, gameGuess), 0)
    (finalGameGuess.score, firstAction)
  }

  def applyAction(action: Action, game: Game): Game = {
    val newGame = action match {
      case gi: GiveInformation => {
        val recipient = game.players(gi.toPlayer.id)
        val newUnknownHand = combineInformation(recipient.unknownHand, gi.handInfo)
        val newRecipient = recipient.copy(unknownHand = newUnknownHand)
        val newPlayer = game.currentPlayer.copy(lastTurn = game.deck.cards.isEmpty)
        val newPlayers = game.players.updated(newPlayer.id, newPlayer).updated(newRecipient.id, newRecipient)
        game.copy(players = newPlayers, numInfoTokens = game.numInfoTokens - 1)
      }
      case dc: DiscardCard => {
        val player = game.currentPlayer
        val removedCard = player.knownHand(dc.cardIndex)
        val (newCardMaybe, newDeck) = game.deck.draw
        val newPlayer = player.swapCard(dc.cardIndex, newCardMaybe).copy(lastTurn = game.deck.cards.isEmpty)
        val newNumInfoTokens = (game.numInfoTokens + 1).min(8)
        game.copy(players = game.players.updated(newPlayer.id, newPlayer), deck = newDeck, discardedCards = game.discardedCards :+ removedCard, numInfoTokens = newNumInfoTokens)
      }
      case pc: PlayCard => {
        val player = game.currentPlayer
        val playedCard = player.knownHand(pc.cardIndex)
        val (newCardMaybe, newDeck) = game.deck.draw
        val newPlayer = player.swapCard(pc.cardIndex, newCardMaybe).copy(lastTurn = game.deck.cards.isEmpty)
        val newPlayers = game.players.updated(newPlayer.id, newPlayer)
        if (game.isPlayLegal(playedCard)) {
          val newFireworks = game.fireworks.updated(playedCard.color, game.fireworks(playedCard.color).addCard(playedCard))
          val newNumInfoTokens = if (playedCard.number == 5) (game.numInfoTokens + 1).min(8) else game.numInfoTokens
          game.copy(players = newPlayers, deck = newDeck, fireworks = newFireworks, numInfoTokens = newNumInfoTokens)
        } else {
          val newNumFuseTokens = game.numFuseTokens - 1
          game.copy(players = newPlayers, deck = newDeck, discardedCards = game.discardedCards :+ playedCard, numFuseTokens = newNumFuseTokens)
        }
      }
    }
    newGame.copy(turn = (newGame.turn + 1) % newGame.players.size)
  }

  def solve(game: Game): Int = {

    def doTrials(game: Game, numTrials: Int): Action = {
      val futures = 1 to numTrials map (_ => Future(doTrial(game)))
      val results = Await.result(Future.sequence(futures), 1.hour)
      val best = results.maxBy(_._1)
      println(s"best is $best")
      best._2
    }

    @tailrec
    def play(game: Game): Game = {
      if (game.isOver) {
        game
      } else {
        println(s"Score: ${game.score}, Info: ${game.numInfoTokens}, Fuses: ${game.numFuseTokens}, Deck: ${game.deck.cards.size}")
        val action = doTrials(game, NumTrials)
        val newGame = applyAction(action, game)
        play(newGame)
      }
    }

    val finishedGame = play(game)
    println(s"Score: ${finishedGame.score}, Info: ${finishedGame.numInfoTokens}, Fuses: ${finishedGame.numFuseTokens}, Deck: ${finishedGame.deck.cards.size}")

    finishedGame.score
  }

  println(s"Final score: ${solve(Game(4))}")
}
