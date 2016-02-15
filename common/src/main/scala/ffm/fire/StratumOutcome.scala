package ffm.fire

import ffm.forest.Stratum

/**
 * Represents the results of an ignition simulation for a particular stratum.
 * 
 * TODO: consider simplifying this and using separate data objects for plant and stratum runs.
 */
trait StratumOutcome {
  
  /** The vegetation stratum which this outcome pertains to. */
  def stratum: Stratum
  
  /** Ignition paths derived from plant ignition simulations. */
  def plantPaths: IndexedSeq[IgnitionPath]
  
  /** Flame series derived from plant ignition simulations. */
  def plantFlameSeries: Option[StratumFlameSeries]
  
  /** Ignition paths derived from stratum ignition simulations. */
  def stratumPaths: IndexedSeq[IgnitionPath]
  
  /** Flame series derived from stratum ignition simulations. */
  def stratumFlameSeries: Option[StratumFlameSeries]
  
  /** 
   * Select one of two flame series based on some criterion (e.g. maximum flame length). 
   *
   * TODO: this method should not be in this data class  
   */
  def selectFlameSeries(f: (StratumFlameSeries, StratumFlameSeries) => StratumFlameSeries): Option[StratumFlameSeries]  
}