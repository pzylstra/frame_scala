package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel

/**
 * Defines the context for an ignition simulation, including site, stratum and weather conditions.
 * 
 * The identity of the species and the starting point for the ignition path are not included here
 * since ignition may be simulated for multiple combinations of these using the same context data. 
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
