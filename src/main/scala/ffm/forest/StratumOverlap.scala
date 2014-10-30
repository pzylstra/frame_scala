package ffm.forest

sealed trait StratumOverlapType
object StratumOverlapType {
  case object Overlapping extends StratumOverlapType
  case object NotOverlapping extends StratumOverlapType
  case object Undefined extends StratumOverlapType

  /**
   * Retrieve a StratumOverlapType by name.
   *
   * Ignores case and any surrounding or embedded spaces and hyphens.
   *
   * {{{
   * val ov1 = StratmOverlapType("not overlapping")
   * val ov2 = StratumOverlapType("notoverlapping")
   *
   * ov1 == ov2  // will be `true`
   * }}}
   */
  def apply(name: String): StratumOverlapType = name.replaceAll("""[\s\-]+""", "").toLowerCase() match {
    case "overlapped" => Overlapping
    case "notoverlapped" => NotOverlapping
    case "automatic" => Undefined
    case s => throw new IllegalArgumentException("Not a valid stratum overlap type: " + s)
  }
}

/**
 * Stores the type of overlap between two strata.
 */
case class StratumOverlap(lower: StratumLevel, upper: StratumLevel, overlapType: StratumOverlapType) {
  require(lower < upper)
}
