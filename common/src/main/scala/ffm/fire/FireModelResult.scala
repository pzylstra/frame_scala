package ffm.fire

import ffm.forest.Site

/**
 * Defines results which may be aggregated from multiple [[FireModelRunResult]]s.
 */
trait FireModelResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Aggregated flame attributes for strata. */
  def stratumResults: IndexedSeq[StratumFlameSummary]
  
  /** Individual run results. */
  def runResults: IndexedSeq[FireModelRunResult]
}
