package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel

/**
 * Results derived from a single run of a fire  model.
 */
trait FireModelRunResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Whether canopy effects on wind were included in this run. */
  def canopyEffectIncluded: Boolean
  
  /** Surface conditions and flames from the run. */
  def surfaceOutcome: SurfaceOutcome
  
  /** Summary flame results for each stratum. */
  def flameSummaries: Map[StratumLevel, StratumFlameSummary]
  
  /** Detailed results for each stratum. */
  def pathsAndFlames: Map[StratumLevel, StratumPathsFlames]
  
  /** Combined flames. */
  def combinedFlames: IndexedSeq[Flame]
  
  /** 
   * Rate of spread for strata other than the canopy (m/s).
   */
  def ratesOfSpread: Map[StratumLevel, Double]
}
