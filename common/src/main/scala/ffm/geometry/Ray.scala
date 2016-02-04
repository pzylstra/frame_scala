package ffm.geometry

import ffm.numerics.Numerics._

/**
 * Represents a semi-infinite line with a given origin and angle.
 */
class Ray private(c0: Coord, theta: Double) {
  
  /**
   * Ray origin coordinate.
   */
  val origin = c0
  
  /**
   * Ray angle in the range [0, 2Pi).
   */
  val angle = Angles.normalizeTwoPi(theta)

  /**
   * Finds the point on this ray at the given distance from the origin.
   */
  def atDistance(distance: Double): Coord =
    origin.toBearing(angle, distance)
  
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
  /** Creates a Ray from origin with the given angle. */
  def apply(origin: Coord, angle: Double): Ray =
    new Ray(origin, angle)
}
