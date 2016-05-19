package ffm.fire

import ffm.forest.Site

/**
 * Holds combined results of ignition runs.
 */
trait FireModelResult {
  /** The site (including conditions and vegetation) for which this run was done. */
  def site: Site
  
  /** Results for the first ignition run with canopy effects on wind included. */
  def resWithCanopyEffect: FireModelRunResult
  
  /** 
   * Results for second ignition run with canopy effect on wind excluded.
   *  
   * This may be an empty object. 
   */
  def resWithoutCanopyEffect: FireModelRunResult
  
  /** 
   * Rate of spread for the canopy stratum.
   * 
   * Note: rates of spread for lower strata will be recorded in
   * the individual run result objects.
   */
  def canopyROS: Double
}
