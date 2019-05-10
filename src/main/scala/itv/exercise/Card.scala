package itv.exercise

sealed trait FeatureValue

sealed trait Feature {
  def allValues: (FeatureValue, FeatureValue, FeatureValue)
}
object Feature {
  def isValidForSet(feature1: FeatureValue, feature2: FeatureValue, feature3: FeatureValue): Boolean =
    feature1 == feature2 && feature2 == feature3 ||
      feature1 != feature2 && feature2 != feature3 && feature1 != feature3
}

sealed trait Colour extends FeatureValue
object Colour extends Feature {
  case object Red    extends Colour
  case object Purple extends Colour
  case object Green  extends Colour
  val allValues = (Red, Purple, Green)
}

sealed trait Number extends FeatureValue
object Number extends Feature {
  case object One   extends Number
  case object Two   extends Number
  case object Three extends Number
  val allValues = (One, Two, Three)
}

sealed trait Shading extends FeatureValue
object Shading extends Feature {
  case object Solid    extends Shading
  case object Striped  extends Shading
  case object Outlined extends Shading
  val allValues = (Solid, Striped, Outlined)
}

sealed trait Shape extends FeatureValue
object Shape extends Feature {
  case object Oval     extends Shape
  case object Squiggle extends Shape
  case object Diamonds extends Shape
  val allValues = (Oval, Squiggle, Diamonds)
}

case class Card(shading: Shading)
