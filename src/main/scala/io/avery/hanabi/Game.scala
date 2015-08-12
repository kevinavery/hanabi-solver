package io.avery.hanabi

import scala.annotation.tailrec
import scala.collection.immutable.Queue
import scala.util.Random

case class Color(value: String) {
  override def toString = value
}
object Colors {
  val Red = Color("red")
  val Yellow = Color("yellow")
  val Green = Color("green")
  val Blue = Color("blue")
  val White = Color("white")

  val AllColors = List(Red, Yellow, Green, Blue, White)
}

trait Information
trait PositiveInformation extends Information
trait NegativeInformation extends Information
case class KnownNumber(num: Int) extends PositiveInformation {
  override def toString = s"$num"
}
case class KnownColor(color: Color) extends PositiveInformation {
  override def toString = s"$color"
}
case class KnownNotNumber(num: Int) extends NegativeInformation {
  override def toString = s"!$num"
}
case class KnownNotColor(color: Color) extends NegativeInformation {
  override def toString = s"!$color"
}


trait Card
case class KnownCard(number: Int, color: Color, id: Int) extends Card {
  override def toString = s"Card($color $number)"
}
case class UnknownCard(info: Set[Information]) extends Card {
  override def toString = s"Card?(${info.mkString(", ")})"
}

case class Deck(cards: Queue[KnownCard]) {
  def reset: Deck = {
    val cards = for {
      number <- List(1, 1, 1, 1, 2, 2, 3, 3, 4, 4, 5)
      color <- Colors.AllColors
    } yield new KnownCard(number, color, 0)
    val indexedCards = cards.zipWithIndex.map(e => e._1.copy(id = e._2))
    Deck(Queue()).queueCards(indexedCards)
  }

  def queueCards(addedCards: Seq[KnownCard]): Deck = {
    Deck(Queue(Random.shuffle(cards ++ addedCards): _*))
  }

  def removeCards(removedCards: Seq[KnownCard]): Deck = {
    val withoutCards = cards.filter(c => !removedCards.exists(rc => c.id == rc.id))
    Deck(withoutCards)
  }

  def draw: (Option[KnownCard], Deck) = {
    cards.dequeueOption match {
      case None => (None, copy())
      case Some((card, cards)) => (Some(card), Deck(cards))
    }
  }

  def dealHand(handSize: Int): (Vector[KnownCard], Deck) = {
    @tailrec
    def accCards(partialCards: Vector[KnownCard], deck: Deck): (Vector[KnownCard], Deck) = {
      if (partialCards.size == handSize) {
        (partialCards, deck)
      } else {
        val (Some(card), newDeck) = deck.draw
        accCards(partialCards :+ card, newDeck)
      }
    }
    accCards(Vector(), copy())
  }
}

case class Player(id: Int, knownHand: Vector[KnownCard], unknownHand: Vector[UnknownCard], lastTurn: Boolean) {
  require(knownHand.size == unknownHand.size)

  def swapCard(cardIndex: Int, cardMaybe: Option[KnownCard]): Player = {
    copy(knownHand = knownHand.patch(cardIndex, cardMaybe.toVector, 1),
      unknownHand = unknownHand.patch(cardIndex, cardMaybe.map(_ => UnknownCard(Set())).toVector, 1))
  }
}

trait Action
case class GiveInformation(player: Player, toPlayer: Player, handInfo: Vector[Information]) extends Action
case class DiscardCard(player: Player, cardIndex: Int) extends Action
case class PlayCard(player: Player, cardIndex: Int) extends Action

case class Firework(color: Color, cards: List[KnownCard]) {
  def addCard(card: KnownCard): Firework = {
    require(card.color == color)
    require(card.number == cards.headOption.map(_.number).getOrElse(0) + 1)
    copy(cards = card :: cards)
  }

  def topCardMaybe: Option[KnownCard] = cards.headOption
}

object Game {
  def apply(numPlayers: Int): Game = {
    @tailrec
    def accPlayers(players: Vector[Player], deck: Deck): (Vector[Player], Deck) = {
      if (players.size == numPlayers) {
        (players, deck)
      } else {
        val (hand, newDeck) = deck.dealHand(4)
        val player = Player(players.size, hand, Vector.fill(4)(UnknownCard(Set())), false)
        accPlayers(players :+ player, newDeck)
      }
    }
    val (players, deck) = accPlayers(Vector(), Deck(Queue()).reset)
    val fireworks = Colors.AllColors.map(c => c -> Firework(c, Nil)).toMap

    new Game(deck, Vector(), 0, players, 8, 3, fireworks)
  }
}

case class Game(deck: Deck, discardedCards: Vector[KnownCard], turn: Int, players: Vector[Player], numInfoTokens: Int, numFuseTokens: Int, fireworks: Map[Color, Firework]) {
  require(numFuseTokens >= 0, "Out of fuses!")
  require(numInfoTokens >= 0, "Out of info tokens!")

  def currentPlayer = players(turn)

  def score: Int = fireworks.values.map(_.cards.size).sum

  def isPlayLegal(card: KnownCard): Boolean = {
    val topCardNumber: Int = fireworks.get(card.color).flatMap(_.topCardMaybe.map(_.number)).getOrElse(0)
    topCardNumber == card.number - 1
  }

  def lastRound = deck.cards.isEmpty

  def isOver = numFuseTokens == 0 || players.forall(_.lastTurn)
}
