package itv.exercise

sealed trait FeatureValue
sealed trait Feature { def allValues: (FeatureValue, FeatureValue, FeatureValue) }

sealed trait Shading extends FeatureValue
object Shading extends Feature {
  case object Solid    extends Shading
  case object Striped  extends Shading
  case object Outlined extends Shading
  val allValues = (Solid, Striped, Outlined)
}

sealed trait Colour extends FeatureValue
object Colour extends Feature {
  case object Red    extends Colour
  case object Purple extends Colour
  case object Green  extends Colour
  val allValues = (Red, Purple, Green)
}

case class Card(shading: Shading)
