package ffm.fire

import ffm.forest.Site
import ffm.forest.Vegetation

/**
 * Results derived from a single run of a fire  model.
 */
trait FireModelRunResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Surface conditions and flames from the run. */
  def surfaceOutcome: SurfaceOutcome
  
  /** Results for each stratum. */
  def stratumOutcomes: IndexedSeq[StratumOutcome]
  
  /** Combined flames
   *  
   *  TODO: document me properly
   */
  def combinedFlames: IndexedSeq[Flame]
}
