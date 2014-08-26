package ffm.forest

sealed trait LeafForm
object LeafForm {
  case object Round extends LeafForm
  case object Flat extends LeafForm
}
