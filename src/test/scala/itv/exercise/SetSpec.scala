package itv.exercise

import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}

class SetSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  def test(card1: Card, card2: Card, card3: Card): Boolean =
    card1.shading == card2.shading && card2.shading == card3.shading ||
      card1.shading != card2.shading && card2.shading != card3.shading

  "A Set of three cards" - {
    "is valid if" - {
      "a feature is all the same" in {
        test(
          card1 = Card(Shading.Solid),
          card2 = Card(Shading.Solid),
          card3 = Card(Shading.Solid)
        ) should be(true)
      }
      "a feature is different in all three cards" in {
        test(
          card1 = Card(Shading.Solid),
          card2 = Card(Shading.Striped),
          card3 = Card(Shading.Outlined)
        ) should be(true)
      }
    }
    "is invalid if" - {
      "a feature is the same for two but different for the third" in {
        test(
          card1 = Card(Shading.Solid),
          card2 = Card(Shading.Solid),
          card3 = Card(Shading.Outlined)
        ) should be(false)
      }
    }
  }
}
