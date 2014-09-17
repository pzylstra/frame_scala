package ffm.fire

import ffm.forest.{Stratum, StratumLevel}
import ffm.geometry.Coord


case class PreHeatingFlame(flame: Flame, level: StratumLevel, startTime: Double, endTime: Double) {
 
  /**
   * The duration (end time - start time). 
   * 
   * Returns 0 if end time is less than start time.
   */
  val duration = math.max(0.0, endTime - startTime)
  
  /**
   * TODO: document this once I understand it
   */
  def duration(altEndTime: Double): Double =
    if (endTime <= 0) duration
    else {
      val d = math.min(endTime, altEndTime) - startTime
      math.max(0.0, d)
    }
  
  /**
   * Returns a new pre-heating flame with the same properties as this one but a 
   * new flame origin.
   */
  def toOrigin(newOrigin: Coord): PreHeatingFlame =
    PreHeatingFlame(flame.toOrigin(newOrigin), level, startTime, endTime)
 
}
