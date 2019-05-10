package itv.exercise

import org.scalacheck.{Arbitrary, Gen, Shrink}
import Arbitrary.arbitrary
import org.scalatest.prop.GeneratorDrivenPropertyChecks
import org.scalatest.{FreeSpec, Matchers}
import org.scalacheck.magnolia._

import scala.util.Random

class FeatureSpec extends FreeSpec with Matchers with GeneratorDrivenPropertyChecks {
  implicit def noShrink[T]: Shrink[T] = Shrink(_ => Stream.empty)

  "A card feature" - {
    "is valid for a Set if" - {
      "they are all the same" in forAll { featureValue: FeatureValue =>
        Feature.isValidForSet(
          feature1 = featureValue,
          feature2 = featureValue,
          feature3 = featureValue
        ) should be(true)
      }
      "they are different in all three cards" in {
        val differentFeatureValuesInRandomOrder = arbitrary[Feature].map(_.allValues).map(shuffleTuple)

        forAll(differentFeatureValuesInRandomOrder) {
          case (_1, _2, _3) =>
            Feature.isValidForSet(
              feature1 = _1,
              feature2 = _2,
              feature3 = _3
            ) should be(true)
        }
      }
    }
    "is invalid if they are the same for two but different for the third" in {
      val differentFeatureValuesInRandomOrder = for {
        feature <- arbitrary[Feature]
        _1      <- Gen.oneOf(asList(feature.allValues))
        _2      <- Gen.oneOf(asList(feature.allValues)) if _1 != _2
      } yield shuffleTuple((_1, _1, _2))

      forAll(differentFeatureValuesInRandomOrder) {
        case (_1, _2, _3) =>
          Feature.isValidForSet(
            feature1 = _1,
            feature2 = _2,
            feature3 = _3
          ) should be(false)
      }
    }
  }

  def shuffleTuple[T](v: (f.Value, f.Value, f.Value) forSome { val f: itv.exercise.Feature }) =
    Random.shuffle(asList(v)) match {
      case List(_1, _2, _3) => (_1, _2, _3)
    }

  private def asList[T](v: (T, T, T)) = {
    List(v._1, v._2, v._3)
  }
}
