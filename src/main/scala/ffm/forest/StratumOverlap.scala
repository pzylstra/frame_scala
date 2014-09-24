package ffm.forest

sealed trait StratumOverlapType
object StratumOverlapType {
  case object Overlapping extends StratumOverlapType
  case object NotOverlapping extends StratumOverlapType
  case object Undefined extends StratumOverlapType
}

/**
 * Stores the type of overlap between two strata.
 */
case class Overlap(lower: StratumLevel, upper: StratumLevel, overlapType: StratumOverlapType) {
  require(lower < upper)
}
