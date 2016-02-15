package ffm.fire

/**
 * Defines fire attributes to be derived from surface data.
 */
trait SurfaceFireAttributes {
  
  /** Rate of spread for backing surface fire (m/s). */
  def backingROS: Double
  
  /** Flame length for a backing surface fire (m). */
  def backingFlameLength: Double
  
  /**
   * Rate of spread for heading surface fire (m/s).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def headROS(surfaceWindSpeed: Double): Double
  
  /**
   * Flame length for heading surface file (m).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def headFlameLength(surfaceWindSpeed: Double): Double

  /**
   * Overall surface fire rate of spread (m/s).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def ros(surfaceWindSpeed: Double): Double

  /**
   * Overall surface fire flame length (m).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def flameLength(surfaceWindSpeed: Double): Double

  /** Surface fire flame residence time (s). */
  def flameResidenceTime: Double
  
}
