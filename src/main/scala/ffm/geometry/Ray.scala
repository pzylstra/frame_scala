package ffm.geometry

import com.vividsolutions.jts.algorithm.Angle

/**
 * Represents a semi-infinite line with a given origin and angle.
 */
class Ray(val origin: Coord, theta: Double) {
  
  /**
   * Ray angle in the range [0, 2Pi).
   */
  val angle = Angle.normalizePositive(theta)
}

object Ray {
  def apply(origin: Coord, angle: Double): Ray =
    new Ray(origin, angle)
}
