package itv.exercise

sealed trait Shading
object Shading {
  case object Solid    extends Shading
  case object Striped  extends Shading
  case object Outlined extends Shading
}

case class Card(shading: Shading)
