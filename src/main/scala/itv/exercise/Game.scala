package itv.exercise

import java.lang.Math.max

case class GameState(cardsOnTable: List[Card], remainingCards: List[Card], points: Map[String, Int])

class Game(private var state: GameState) {
  val cardsOnTable = state.cardsOnTable
  val points       = state.points

  def submitSet(player: String, card1: Card, card2: Card, card3: Card): Game.SubmitResult =
    if (!CardSet.validate(card1, card2, card3))
      Game.SubmitResult.Invalid(
        "not a valid set",
        new Game(state.copy(points = state.points.updated(player, max(state.points(player) - 1, 0))))
      )
    else if (!(cardsOnTable.contains(card1) || cardsOnTable.contains(card2) || cardsOnTable.contains(card3)))
      Game.SubmitResult.Invalid("cards not found on table", this)
    else
      Game.SubmitResult.Valid(
        new Game(
          GameState(
            cardsOnTable = cardsOnTable.filterNot(Set(card1, card2, card3).contains) ::: state.remainingCards.take(3),
            remainingCards = state.remainingCards.drop(3),
            points = state.points.updated(player, state.points(player) + 1)
          )
        )
      )
}
object Game {
  sealed trait SubmitResult { def game: Game }
  object SubmitResult {
    case class Valid(game: Game)                   extends SubmitResult
    case class Invalid(reason: String, game: Game) extends SubmitResult
  }

  def apply(cardsInDealingOrder: List[Card], players: Set[String]): Game =
    new Game(
      GameState(
        cardsOnTable = cardsInDealingOrder.take(12),
        remainingCards = cardsInDealingOrder.drop(12),
        points = players.map(_ -> 0).toMap
      )
    )
}
