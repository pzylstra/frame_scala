package ffm.util

/**
 * Conversions between units of measurement.
 */
object Units {
  
  sealed trait Unit

  /**
   * Convert from km/h to m/s.
   */
  def kph2mps(kph: Double) = kph * 5 / 18
  
  /**
   * Convert form m/s to km/h
   */
  def mps2kph(mps: Double) = mps * 18 / 5
  
  /**
   * Convert from degrees to radians.
   */
  def deg2rad(degrees: Double) = math.toRadians(degrees)
  
  /**
   * Convert from radians to degrees.
   */
  def rad2deg(radians: Double) = math.toDegrees(radians)
    
}