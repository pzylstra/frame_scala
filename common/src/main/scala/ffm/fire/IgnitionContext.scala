package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel

/**
 * Defines the context for an ignition simulation, including site, stratum and weather conditions.
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
