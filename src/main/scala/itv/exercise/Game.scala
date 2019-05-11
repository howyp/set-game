package itv.exercise

case class GameState(cardsOnTable: List[Card], remainingCards: List[Card])

class Game(private var state: GameState) {
  val cardsOnTable = state.cardsOnTable
  def callSet(card1: Card, card2: Card, card3: Card) =
    GameState(cardsOnTable = cardsOnTable.filterNot(Set(card1, card2, card3).contains) ::: state.remainingCards.take(3),
              remainingCards = cardsOnTable)
}
object Game {
  def apply(cardsInDealingOrder: List[Card]): Game =
    new Game(GameState(cardsOnTable = cardsInDealingOrder.take(12), remainingCards = cardsInDealingOrder.drop(12)))
}
