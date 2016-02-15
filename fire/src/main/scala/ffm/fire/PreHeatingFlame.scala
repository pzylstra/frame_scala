package ffm.fire

import ffm.forest.StratumLevel
import ffm.geometry.Coord

object DefaultPreHeatingFlame {

  /** Creates a new PreHeatingFlame. */
  def apply(f: Flame, level: StratumLevel, startTime: Double, endTime: Double): PreHeatingFlame =
    new DefaultImpl(f, level, startTime, endTime)
  
  /** 
   * Creates a new PreHeatingFlame with the same attributes as the given flame
   * but a different origin.
   */
  def toOrigin(phf: PreHeatingFlame, newOrigin: Coord): PreHeatingFlame = {
    val f = DefaultFlame.toOrigin(phf.flame, newOrigin)
    new DefaultImpl(f, phf.level, phf.startTime, phf.endTime)
  }
  
  private class DefaultImpl(
      val flame: Flame,
      val level: StratumLevel,
      val startTime: Double,
      val endTime: Double) extends PreHeatingFlame {

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
  }
}
