package itv.exercise

case class GameState(cardsOnTable: List[Card])

class Game(cardsInDealingOrder: List[Card]) {
  val cardsOnTable = cardsInDealingOrder
  def callSet(card1: Card, card2: Card, card3: Card) =
    GameState(cardsOnTable.filterNot(Set(card1, card2, card3).contains))
}
