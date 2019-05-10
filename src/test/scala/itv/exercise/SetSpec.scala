package itv.exercise

import org.scalacheck.{Arbitrary, Gen, Shrink}
import Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.magnolia._

import scala.util.Random

class SetSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink(_ => Stream.empty)

  def test(card1: Card, card2: Card, card3: Card): Boolean =
    card1.shading == card2.shading && card2.shading == card3.shading ||
      card1.shading != card2.shading && card2.shading != card3.shading && card1.shading != card3.shading

  "A Set of three cards" - {
    "is valid if" - {
      "a feature is all the same" in forAll { shading: Shading =>
        test(
          card1 = Card(shading),
          card2 = Card(shading),
          card3 = Card(shading)
        ) should be(true)
      }
      "a feature is different in all three cards" in {
        val differentFeaturesInRandomOrder =
          Gen.const(List(Shading.Solid, Shading.Striped, Shading.Outlined)).map(Random.shuffle(_))

        forAll(differentFeaturesInRandomOrder) {
          case List(_1, _2, _3) =>
            test(
              card1 = Card(_1),
              card2 = Card(_2),
              card3 = Card(_3)
            ) should be(true)
        }
      }
    }
    "is invalid if" - {
      "a feature is the same for two but different for the third" in {
        val differentShadingsInRandomOrder = for {
          _1 <- arbitrary[Shading]
          _2 <- arbitrary[Shading] if _1 != _2
        } yield Random.shuffle(List(_1, _1, _2))

        forAll(differentShadingsInRandomOrder) {
          case List(_1, _2, _3) =>
            test(
              card1 = Card(_1),
              card2 = Card(_2),
              card3 = Card(_3)
            ) should be(false)
        }
      }
    }
  }
}
