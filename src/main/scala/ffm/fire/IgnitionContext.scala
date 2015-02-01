package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel

/**
 * Type of ignition run: either `PlantRun` (individual plant canopy) or
 * `StratumRun` (fire spread across multiple canopies).
 */
sealed trait IgnitionRunType
object IgnitionRunType {
  case object PlantRun extends IgnitionRunType
  case object StratumRun extends IgnitionRunType
}

/**
 * Defines the context, including site, stratum and weather conditions, for
 * modelling an ignition event.
 */
case class IgnitionContext(
  runType: IgnitionRunType,
  site: Site,
  stratumLevel: StratumLevel,
  preHeatingFlames: IndexedSeq[PreHeatingFlame],
  incidentFlames: IndexedSeq[Flame],
  preHeatingEndTime: Double,
  canopyHeatingDistance: Double,
  stratumWindSpeed: Double)

  
