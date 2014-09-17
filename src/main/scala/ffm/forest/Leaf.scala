package ffm.forest

sealed trait LeafForm
object LeafForm {
  case object Round extends LeafForm
  case object Flat extends LeafForm
  
  /**
   * Retrieve a LeafForm by name.
   * 
   * Ignores case and surrounding space.
   */
  def apply(name: String): LeafForm = name.trim().toLowerCase() match {
    case "round" => Round
    case "flat" => Flat
    case s => throw new IllegalArgumentException("Not a valid leaf form name: " + s)
  }
}
