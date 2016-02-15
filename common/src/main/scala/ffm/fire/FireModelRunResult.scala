package ffm.fire

/**
 * Results derived from a single run of a fire  model.
 */
trait FireModelRunResult {
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
