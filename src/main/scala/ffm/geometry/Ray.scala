package ffm.geometry

import ffm.numerics.Numerics._

/**
 * Represents a semi-infinite line with a given origin and angle.
 */
class Ray(val origin: Coord, theta: Double) {
  
  /**
   * Ray angle in the range [0, 2Pi).
   */
  val angle = Angles.normalizeTwoPi(theta)

  /**
   * Returns `true` if the given Ray has (almost) the same origin and angle
   * as this Ray.
   */
  def almostEq(that: Ray): Boolean =
    origin.almostEq(that.origin) && Angles.almostEq(angle, that.angle)
  
  override def toString =
    f"Ray(origin=$origin, angle=${angle}%.8f)"
}

object Ray {
  def apply(origin: Coord, angle: Double): Ray =
    new Ray(origin, angle)
}
