package itv.exercise

import org.scalatest.{FreeSpec, Matchers}
import Colour._
import Number._
import Shading._
import Shape._
import itv.exercise.Game.SubmitResult.{Invalid, Valid}

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
      val Valid(gameAfterMove1) = initialGame.submitSet("player1", validSet1(0), validSet1(1), validSet1(2))
      gameAfterMove1.cardsOnTable should contain noneOf (validSet1(0), validSet1(1), validSet1(2))
      gameAfterMove1.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: validSet3)
      gameAfterMove1.points should be(Map("player1" -> 1, "player2" -> 0, "player3" -> 0))

      info("Player 2 submits an invalid set, and stays on 0 points")
      val Invalid(reason1, gameAfterMove2) =
        gameAfterMove1.submitSet("player2", invalidSet1(0), invalidSet1(1), invalidSet1(2))
      gameAfterMove2.points should be(Map("player1" -> 1, "player2" -> 0, "player3" -> 0))
      reason1 should be("not a valid set")

      info("Player 1 submits a set which includes cards not yet on the table")
      val Invalid(reason2, gameAfterMove3) =
        gameAfterMove2.submitSet("player1", validSet4(0), validSet4(1), validSet4(2))
      reason2 should be("cards not found on table")

      info("Player 2 submits two valid sets and gets two points")
      val Valid(gameAfterMove4) = gameAfterMove3.submitSet("player2", validSet3(0), validSet3(1), validSet3(2))
      gameAfterMove4.points should be(Map("player1" -> 1, "player2" -> 1, "player3" -> 0))
      gameAfterMove4.cardsOnTable should be(invalidSet1 ::: validSet2 ::: invalidSet2 ::: invalidSet3)

      val Valid(gameAfterMove5) = gameAfterMove4.submitSet("player2", validSet2(0), validSet2(1), validSet2(2))
      gameAfterMove5.points should be(Map("player1" -> 1, "player2" -> 2, "player3" -> 0))
      gameAfterMove5.cardsOnTable should be(invalidSet1 ::: invalidSet2 ::: invalidSet3 ::: validSet4)

      info("Player 2 submits an invalid set, and loses a point")
      val Invalid(reason3, gameAfterMove6) =
        gameAfterMove5.submitSet("player2", invalidSet1(0), invalidSet1(1), invalidSet1(2))
      reason3 should be("not a valid set")
      gameAfterMove6.points should be(Map("player1" -> 1, "player2" -> 1, "player3" -> 0))
      gameAfterMove6.cardsOnTable should be(gameAfterMove5.cardsOnTable)

      info("Player 1 submits a valid set, and no more cards are added because the deck is depleted")
      val Valid(gameAfterMove7) = gameAfterMove6.submitSet("player1", validSet4(0), validSet4(1), validSet4(2))
      gameAfterMove7.points should be(Map("player1" -> 2, "player2" -> 1, "player3" -> 0))
      gameAfterMove7.cardsOnTable should be(invalidSet1 ::: invalidSet2 ::: invalidSet3)

      info("The players declare that no valid sets are left and the game ends")
      gameAfterMove7.noValidSetsFound() should be(Game.GameOver(winningPlayer = "player1"))
    }
  }
}
