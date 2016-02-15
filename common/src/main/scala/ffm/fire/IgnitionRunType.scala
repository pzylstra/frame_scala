package ffm.fire

/**
 * Type of ignition run: either `PlantRun` (individual plant crown) or
 * `StratumRun` (fire spread across multiple crowns).
 */
sealed trait IgnitionRunType

object IgnitionRunType {
  case object PlantRun extends IgnitionRunType
  case object StratumRun extends IgnitionRunType
}
