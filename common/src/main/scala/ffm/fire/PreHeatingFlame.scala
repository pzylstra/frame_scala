package ffm.fire

import ffm.forest.StratumLevel

/**
 * A specialized type of flame used in the pre-heating phase of an ignition simulation.
 * 
 * TODO: could this inherit from Flame instead of being composed ?
 */
trait PreHeatingFlame {
  
  /** The base flame. */
  def flame: Flame
  
  /** The stratum level which this flame is acting on. */
  def level: StratumLevel
  
  /** Start time. */
  def startTime: Double
  
  /** End time. */
  def endTime: Double
  
  /** 
   * Pre-heating duration.
   *  
   * Returns 0 if end time is less than start time.
   */
  def duration: Double
  
  /**
   * Alternative duration.
   * 
   * TODO - how is this used ?
   */
  def duration(altEndTime: Double): Double
  
}