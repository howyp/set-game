package itv.exercise

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.magnolia._

import scala.util.Random

class CardSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Three cards are" - {
    def validCombinationFor(feature: Feature) = Gen.oneOf(
      Gen.oneOf(asList3(feature.allValues)).map(v => (v, v, v)),
      Gen.const(feature.allValues).map(shuffleTuple)
    )
    def invalidCombinationFor(feature: Feature) =
      for {
        first  <- Gen.oneOf(asList3(feature.allValues))
        second <- Gen.oneOf(asList3(feature.allValues)) if first != second
      } yield shuffleTuple((first, first, second))
    def combinationFor(feature: Feature)(valid: Boolean): Gen[(feature.Value, feature.Value, feature.Value)] =
      if (valid) validCombinationFor(feature) else invalidCombinationFor(feature)

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
        Set.validate(
          card1 = Card(colour1, number1, shading1, shape1),
          card2 = Card(colour2, number2, shading2, shape2),
          card3 = Card(colour3, number3, shading3, shape3)
        ) should be(true)
    }

    val atLeastOneInvalidCombination =
      for {
        (colourValid, numberValid, shadingValid, shapeValid) <- fourBooleansWithAtLeastOneFalse
        colour                                               <- combinationFor(Colour)(colourValid)
        number                                               <- combinationFor(Number)(numberValid)
        shading                                              <- combinationFor(Shading)(shadingValid)
        shape                                                <- combinationFor(Shape)(shapeValid)
      } yield (colour, number, shading, shape)

    "a invalid set if one feature is invalid" in forAll(atLeastOneInvalidCombination) {
      case ((colour1, colour2, colour3),
            (number1, number2, number3),
            (shading1, shading2, shading3),
            (shape1, shape2, shape3)) =>
        Set.validate(
          card1 = Card(colour1, number1, shading1, shape1),
          card2 = Card(colour2, number2, shading2, shape2),
          card3 = Card(colour3, number3, shading3, shape3)
        ) should be(false)
    }
  }

  private def fourBooleansWithAtLeastOneFalse =
    Gen.zip(Gen.const(false), arbitrary[Boolean], arbitrary[Boolean], arbitrary[Boolean]).map(shuffleTuple4)

  private def shuffleTuple[T](v: (T, T, T)): (T, T, T) =
    Random.shuffle(asList3(v)) match {
      case List(_1, _2, _3) => (_1, _2, _3)
    }

  private def asList3[T](v: (T, T, T)) = {
    List(v._1, v._2, v._3)
  }

  private def shuffleTuple4[T](v: (T, T, T, T)): (T, T, T, T) =
    Random.shuffle(asList4(v)) match {
      case List(_1, _2, _3, _4) => (_1, _2, _3, _4)
    }

  private def asList4[T](v: (T, T, T, T)) = {
    List(v._1, v._2, v._3, v._4)
  }
}
