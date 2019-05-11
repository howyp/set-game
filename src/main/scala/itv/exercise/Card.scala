package itv.exercise

sealed trait FeatureValue

sealed trait Feature {
  type Value <: FeatureValue
  def allValues: (Value, Value, Value)
}

sealed trait Colour extends FeatureValue
object Colour extends Feature {
  type Value = Colour

  case object Red    extends Colour
  case object Purple extends Colour
  case object Green  extends Colour
  val allValues = (Red, Purple, Green)
}

sealed trait Number extends FeatureValue
object Number extends Feature {
  type Value = Number

  case object One   extends Number
  case object Two   extends Number
  case object Three extends Number
  val allValues = (One, Two, Three)
}

sealed trait Shading extends FeatureValue
object Shading extends Feature {
  type Value = Shading

  case object Solid    extends Shading
  case object Striped  extends Shading
  case object Outlined extends Shading
  val allValues = (Solid, Striped, Outlined)
}

sealed trait Shape extends FeatureValue
object Shape extends Feature {
  type Value = Shape

  case object Oval     extends Shape
  case object Squiggle extends Shape
  case object Diamonds extends Shape
  val allValues = (Oval, Squiggle, Diamonds)
}

case class Card(
    colour: Colour,
    number: Number,
    shading: Shading,
    shape: Shape
)

object Set {

  def validate(card1: Card, card2: Card, card3: Card): Boolean = {
    def validate[F <: FeatureValue](f: Card => F): Boolean = {
      val feature1 = f(card1)
      val feature2 = f(card2)
      val feature3 = f(card3)
      (feature1 == feature2 && feature2 == feature3) ||
      (feature1 != feature2 && feature2 != feature3 && feature1 != feature3)
    }

    validate(_.colour) &&
    validate(_.number) &&
    validate(_.shading) &&
    validate(_.shape)
  }

}
