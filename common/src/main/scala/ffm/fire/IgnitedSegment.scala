package ffm.fire

import ffm.geometry.Coord
import ffm.numerics.Numerics

/** 
 * Represents an ignited segment between two points at a given time step. 
 */
case class IgnitedSegment(timeStep: Int, start: Coord, end: Coord) {
  val length = start.distanceTo(end)
  require (Numerics.Distance.gt(length, 0.0), "Ignited segment length must be greater than 0")

  /**
   * Override equals so that the start and end coordinates are compared
   * robustly.
   */
  override def equals(other: Any) = {
    other match {
      // comparison with another IgnitedSegment
      case that: ffm.fire.IgnitedSegment =>
        that.canEqual(IgnitedSegment.this) &&
          timeStep == that.timeStep &&
          start.almostEq(that.start) &&
          end.almostEq(that.end)

      // comparison with anything else
      case _ => false
    }
  }

}
