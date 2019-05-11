package itv.exercise

case class GameState(cardsOnTable: List[Card], remainingCards: List[Card], points: Map[String, Int])

class Game(private var state: GameState) {
  val cardsOnTable = state.cardsOnTable
  val points       = state.points

  def submitSet(player: String, card1: Card, card2: Card, card3: Card): Either[String, Game] =
    if (CardSet.validate(card1, card2, card3))
      Right(
        new Game(
          GameState(
            cardsOnTable = cardsOnTable.filterNot(Set(card1, card2, card3).contains) ::: state.remainingCards.take(3),
            remainingCards = cardsOnTable,
            points = state.points.updated(player, state.points(player) + 1)
          )
        )
      )
    else Left("not a valid set")
}
object Game {
  def apply(cardsInDealingOrder: List[Card], players: Set[String]): Game =
    new Game(
      GameState(
        cardsOnTable = cardsInDealingOrder.take(12),
        remainingCards = cardsInDealingOrder.drop(12),
        points = players.map(_ -> 0).toMap
      )
    )
}
