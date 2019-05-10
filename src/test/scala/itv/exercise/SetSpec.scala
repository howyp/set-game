package itv.exercise

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class SetSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  case class Card(shading: String)
  def test(card1: Card, card2: Card, card3: Card): Boolean =
    card1.shading == card2.shading && card2.shading == card3.shading ||
      card1.shading != card2.shading && card2.shading != card3.shading

  "A Set of three cards" - {
    "is valid if" - {
      "a feature is all the same" in {
        test(
          card1 = Card("solid"),
          card2 = Card("solid"),
          card3 = Card("solid")
        ) should be(true)
      }
      "a feature is different in all three cards" in {
        test(
          card1 = Card("solid"),
          card2 = Card("striped"),
          card3 = Card("outlined")
        ) should be(true)
      }
    }
    "is invalid if" - {
      "a feature is the same for two but different for the third" in {
        test(
          card1 = Card("solid"),
          card2 = Card("solid"),
          card3 = Card("outlined")
        ) should be(false)
      }
    }
  }
}
