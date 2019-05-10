package itv.exercise

import org.scalatest.{FreeSpec, Matchers}

class CardSpec extends FreeSpec with Matchers {
  def test(card1: Card, card2: Card, card3: Card): Boolean =
    card1 == card2 && card2 == card3 || Feature.isValidForSet(card1.shading, card2.shading, card3.shading)

  "Three cards are" - {
    "a valid set if all the features are valid" in {
      val card = Card(Colour.Red, Number.Two, Shading.Outlined, Shape.Oval)
      test(card, card, card) should be(true)
    }
    "a valid set if one feature is different in every card" in {
      val card = Card(Colour.Red, Number.Two, Shading.Outlined, Shape.Oval)
      test(
        card1 = card,
        card2 = card.copy(shading = Shading.Striped),
        card3 = card.copy(shading = Shading.Solid)
      ) should be(true)
    }
    "a invalid set if one feature is different in only one card" in {
      val card = Card(Colour.Red, Number.Two, Shading.Outlined, Shape.Oval)
      test(
        card1 = card,
        card2 = card,
        card3 = card.copy(shading = Shading.Solid)
      ) should be(false)
    }
  }
}
