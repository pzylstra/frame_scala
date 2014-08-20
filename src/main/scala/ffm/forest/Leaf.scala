package ffm.forest

object Leaf {

  sealed trait Form
  case object Round extends Form
  case object Flat extends Form
}