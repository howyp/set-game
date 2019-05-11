package itv.exercise

import java.lang.Math.max

case class Game(cardsOnTable: List[Card], remainingCards: List[Card], points: Map[String, Int]) {
  def submitSet(player: String, card1: Card, card2: Card, card3: Card): Game.SubmitResult =
    if (!CardSet.validate(card1, card2, card3))
      Game.SubmitResult.Invalid(
        "not a valid set",
        this.copy(points = points.updated(player, max(points(player) - 1, 0)))
      )
    else if (!(cardsOnTable.contains(card1) || cardsOnTable.contains(card2) || cardsOnTable.contains(card3)))
      Game.SubmitResult.Invalid("cards not found on table", this)
    else
      Game.SubmitResult.Valid(
        Game(
          cardsOnTable = cardsOnTable.filterNot(Set(card1, card2, card3).contains) ::: remainingCards.take(3),
          remainingCards = remainingCards.drop(3),
          points = points.updated(player, points(player) + 1)
        )
      )

  def noValidSetsFound() = Game.GameOver(winningPlayer = points.maxBy(_._2)._1)
}
object Game {
  sealed trait SubmitResult
  object SubmitResult {
    case class Valid(game: Game)                   extends SubmitResult
    case class Invalid(reason: String, game: Game) extends SubmitResult
  }

  case class GameOver(winningPlayer: String)

  def apply(cardsInDealingOrder: List[Card], players: Set[String]): Game =
    Game(
      cardsOnTable = cardsInDealingOrder.take(12),
      remainingCards = cardsInDealingOrder.drop(12),
      points = players.map(_ -> 0).toMap
    )
}
