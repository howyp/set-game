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
    val invalidSet3 = List(
      Card(Green, One, Solid, Squiggle),
      Card(Red, One, Solid, Squiggle),
      Card(Red, One, Solid, Diamonds)
    )
    val shuffledCards = validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3 ::: invalidSet3
    val initialGame   = Game(shuffledCards, Set("player1", "player2", "player3"))

    "Starts with dealing the first 12 cards onto the table" in {
      initialGame.cardsOnTable should have(size(12))
      initialGame.cardsOnTable should be(validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2)
    }
    "Starts with 0 points for all players" in {
      initialGame.points should be(Map("player1" -> 0, "player2" -> 0, "player3" -> 0))
    }
    "Allows a player to submit a valid set, which removes those cards from the table, adding 3 more" in {
      val Right(updatedGame) = initialGame.submitSet("player1", validSet1(0), validSet1(1), validSet1(2))
      updatedGame.cardsOnTable should contain noneOf (validSet1(0), validSet1(1), validSet1(2))
      updatedGame.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3)
      updatedGame.points should be(Map("player1" -> 1, "player2" -> 0, "player3" -> 0))
    }
    "Rejects an invalid set when submitted" in {
      initialGame.submitSet("player1", invalidSet1(0), invalidSet1(1), invalidSet1(2)) should be(
        Left("not a valid set"))
    }
  }
}
