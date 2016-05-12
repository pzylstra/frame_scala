package ffm.fire

import ffm.forest.Site

/**
 * Defines results which may be aggregated from multiple [[FireModelRunResult]]s.
 */
trait FireModelResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Results for first ignition run. */
  def run1: FireModelRunResult
  
  /** Results for second ignition run (may be an empty object). */
  def run2: FireModelRunResult
}
