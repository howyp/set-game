package itv.exercise

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.magnolia._

import scala.util.Random

class CardSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  def test(card1: Card, card2: Card, card3: Card): Boolean =
    card1 == card2 && card2 == card3 || Feature.isValidForSet(card1.shading, card2.shading, card3.shading)

  "Three cards are" - {
    def validCombinationFor(feature: Feature) = Gen.oneOf(
      Gen.oneOf(asList(feature.allValues)).map(v => (v, v, v)),
      Gen.const(feature.allValues).map(shuffleTuple)
    )

    "a valid set if all features are valid" in forAll(
      validCombinationFor(Colour),
      validCombinationFor(Number),
      validCombinationFor(Shading),
      validCombinationFor(Shape)
    ) {
      case ((colour1, colour2, colour3),
            (number1, number2, number3),
            (shading1, shading2, shading3),
            (shape1, shape2, shape3)) =>
        test(
          card1 = Card(colour1, number1, shading1, shape1),
          card2 = Card(colour2, number2, shading2, shape2),
          card3 = Card(colour3, number3, shading3, shape3)
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
  def shuffleTuple[T](v: (T, T, T)): (T, T, T) =
    Random.shuffle(asList(v)) match {
      case List(_1, _2, _3) => (_1, _2, _3)
    }

  private def asList[T](v: (T, T, T)) = {
    List(v._1, v._2, v._3)
  }
}
