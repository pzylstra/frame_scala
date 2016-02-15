package ffm.fire

import ffm.forest.StratumLevel

/**
 * Holds summary data for flame attributes in a stratum, distilled from
 * [[StratumOutcome]] and [[StratumFlameSeries]] objects.
 */
case class StratumFlameSummary(
    level: StratumLevel, 
    flameLength: Double, 
    flameAngle: Double, 
    flameHeight: Double)
