package ffm.fire

/**
 * Defines results which may be aggregated from multiple [[FireModelRunResult]]s.
 */
trait FireModelResult {
  def stratumResults: IndexedSeq[StratumFlameSummary]
}
