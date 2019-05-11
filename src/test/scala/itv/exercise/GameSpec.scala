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
    val validSet4 = List(
      Card(Green, Three, Striped, Squiggle),
      Card(Green, Three, Striped, Squiggle),
      Card(Green, Three, Striped, Squiggle)
    )
    val invalidSet1 = List(
      Card(Purple, One, Solid, Diamonds),
      Card(Red, One, Striped, Diamonds),
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
    val shuffledCards = validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3 ::: invalidSet3 ::: validSet4
    val initialGame   = Game(shuffledCards, Set("player1", "player2", "player3"))

    "Starts with dealing the first 12 cards onto the table" in {
      initialGame.cardsOnTable should have(size(12))
      initialGame.cardsOnTable should be(validSet1 ::: invalidSet1 ::: validSet2 ::: invalidSet2)
    }
    "Starts with 0 points for all players" in {
      initialGame.points should be(Map("player1" -> 0, "player2" -> 0, "player3" -> 0))
    }
    "Gameplay progresses:" in {
      info(
        "Player 1 submits a valid set, gaining a point, which removes those cards from the table, adding 3 more cards")
      val Right(gameAfterMove1) = initialGame.submitSet("player1", validSet1(0), validSet1(1), validSet1(2))
      gameAfterMove1.cardsOnTable should contain noneOf (validSet1(0), validSet1(1), validSet1(2))
      gameAfterMove1.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3)
      gameAfterMove1.points should be(Map("player1" -> 1, "player2" -> 0, "player3" -> 0))

      info("Player 2 submits an invalid set, and stays on 0 points")
      gameAfterMove1.submitSet("player1", invalidSet1(0), invalidSet1(1), invalidSet1(2)) should be(
        Left("not a valid set"))

      info("Player 1 submits a set which includes cards not yet on the table")
      gameAfterMove1.submitSet("player1", validSet4(0), validSet4(1), validSet4(2)) should be(
        Left("cards not found on table"))

      info("Player 2 submits two valid sets and gets two points")
      val Right(gameAfterMove4) = gameAfterMove1.submitSet("player2", validSet3(0), validSet3(1), validSet3(2))
      gameAfterMove4.points should be(Map("player1" -> 1, "player2" -> 1, "player3" -> 0))
      gameAfterMove4.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: invalidSet3)

      val Right(gameAfterMove5) = gameAfterMove4.submitSet("player2", validSet2(0), validSet2(1), validSet2(2))
      gameAfterMove5.points should be(Map("player1" -> 1, "player2" -> 2, "player3" -> 0))
      gameAfterMove5.cardsOnTable should be(invalidSet1 ::: invalidSet2 ::: invalidSet3 ::: validSet4)
    }
  }
}
