package ffm.fire

/**
 * Defines results which may be aggregated from multiple [[FireModelRunResult]]s.
 */
trait FireModelResult {
  /**
   * Aggregated flame attributes for strata.
   */
  def stratumResults: IndexedSeq[StratumFlameSummary]
  
  /**
   * Individual run results.
   */
  def runResults: IndexedSeq[FireModelRunResult]
}
