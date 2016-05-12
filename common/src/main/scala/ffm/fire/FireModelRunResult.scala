package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel

/**
 * Results derived from a single run of a fire  model.
 */
trait FireModelRunResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Whether the canopy stratum was included for wind calculations in this run. */
  def canopyIncluded: Boolean
  
  /** Surface conditions and flames from the run. */
  def surfaceOutcome: SurfaceOutcome
  
  /** Summary flame results for each stratum. */
  def flameSummaries: Map[StratumLevel, StratumFlameSummary]
  
  /** Detailed results for each stratum. */
  def pathsAndFlames: Map[StratumLevel, StratumPathsFlames]
  
  /** Combined flames. */
  def combinedFlames: IndexedSeq[Flame]
  
  /** Rate of spread for each stratum (m/s). */
  def ratesOfSpread: Map[StratumLevel, Double]
}
