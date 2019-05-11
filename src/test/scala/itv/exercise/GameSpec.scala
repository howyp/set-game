package itv.exercise

import org.scalatest.{FreeSpec, Matchers}
import Colour._
import Number._
import Shading._
import Shape._

class GameSpec extends FreeSpec with Matchers {
  "A game of Set" - {
    val validSet1 = List(
      Card(Green, Three, Striped, Diamonds),
      Card(Purple, Three, Striped, Diamonds),
      Card(Red, Three, Striped, Diamonds)
    )
    val validSet2 = List(
      Card(Red, Three, Outlined, Oval),
      Card(Green, Two, Striped, Diamonds),
      Card(Purple, One, Solid, Squiggle)
    )
    val validSet3 = List(
      Card(Red, One, Outlined, Diamonds),
      Card(Red, Three, Solid, Squiggle),
      Card(Red, Two, Striped, Oval)
    )
    val invalidSet1 = List(
      Card(Purple, One, Solid, Diamonds),
      Card(Red, One, Outlined, Diamonds),
      Card(Green, Two, Outlined, Squiggle)
    )
    val invalidSet2 = List(
      Card(Purple, Three, Striped, Oval),
      Card(Purple, Three, Solid, Squiggle),
      Card(Purple, Three, Outlined, Oval)
    )
    val shuffledCards = validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3
    val initialGame   = Game(shuffledCards)

    "Starts with dealing a the first 12 cards onto the table" in {
      initialGame.cardsOnTable should have(size(12))
      initialGame.cardsOnTable should be(validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2)
    }
    "Allows a player to submit a valid set, which removes those cards from the table" in {
      val updatedGame = initialGame.callSet(validSet1(0), validSet1(1), validSet1(2))
      updatedGame.cardsOnTable should contain noneOf (validSet1(0), validSet1(1), validSet1(2))
      updatedGame.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3)
    }
  }
}