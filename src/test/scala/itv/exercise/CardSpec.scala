package itv.exercise

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.magnolia._

import scala.util.Random

class CardSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {

  "Three cards are" - {
    "a valid set if all features are valid" in forAll(validFeatureCombinations.map(featuresToCards)) {
      case (card1, card2, card3) => Set.validate(card1, card2, card3) should be(true)
    }

    "a invalid set if one feature is invalid" in forAll(combinationsWithAtLeastOneInvalidFeature.map(featuresToCards)) {
      case (card1, card2, card3) => Set.validate(card1, card2, card3) should be(false)
    }
  }

  val validFeatureCombinations = Gen.zip(
    validCombinationFor(Colour),
    validCombinationFor(Number),
    validCombinationFor(Shading),
    validCombinationFor(Shape)
  )

  val combinationsWithAtLeastOneInvalidFeature =
    for {
      (colourValid, numberValid, shadingValid, shapeValid) <- fourBooleansWithAtLeastOneFalseInAnyOrder
      colour                                               <- combinationFor(Colour)(colourValid)
      number                                               <- combinationFor(Number)(numberValid)
      shading                                              <- combinationFor(Shading)(shadingValid)
      shape                                                <- combinationFor(Shape)(shapeValid)
    } yield (colour, number, shading, shape)

  private def combinationFor(feature: Feature)(valid: Boolean): Gen[(feature.Value, feature.Value, feature.Value)] =
    if (valid) validCombinationFor(feature) else invalidCombinationFor(feature)

  private def validCombinationFor(feature: Feature) = {
    val allValues                     = feature.allValues
    val combinationsWhereAllSame      = Gen.oneOf(asList(allValues)).map(v => (v, v, v))
    val combinationsWhereAllDifferent = Gen.const(allValues).map(shuffleTuple)
    Gen.oneOf(combinationsWhereAllSame, combinationsWhereAllDifferent)
  }

  private def invalidCombinationFor(feature: Feature) =
    for {
      duplicated    <- Gen.oneOf(asList(feature.allValues))
      nonDuplicated <- Gen.oneOf(asList(feature.allValues)) if duplicated != nonDuplicated
    } yield shuffleTuple((duplicated, duplicated, nonDuplicated))

  private val featuresToCards: (
      (
          (Colour.Value, Colour.Value, Colour.Value),
          (Number.Value, Number.Value, Number.Value),
          (Shading.Value, Shading.Value, Shading.Value),
          (Shape.Value, Shape.Value, Shape.Value)
      )) => (Card, Card, Card) = {
    case ((colour1, colour2, colour3),
          (number1, number2, number3),
          (shading1, shading2, shading3),
          (shape1, shape2, shape3)) =>
      (
        Card(colour1, number1, shading1, shape1),
        Card(colour2, number2, shading2, shape2),
        Card(colour3, number3, shading3, shape3)
      )
  }

  private def fourBooleansWithAtLeastOneFalseInAnyOrder =
    Gen
      .zip(Gen.const(false), arbitrary[Boolean], arbitrary[Boolean], arbitrary[Boolean])
      .map(v => Random.shuffle(List(v._1, v._2, v._3, v._4)))
      .map { case List(_1, _2, _3, _4) => (_1, _2, _3, _4) }

  private def shuffleTuple[T](v: (T, T, T)) = Random.shuffle(asList(v)) match { case List(_1, _2, _3) => (_1, _2, _3) }
  private def asList[T](v: (T, T, T))       = List(v._1, v._2, v._3)
}
