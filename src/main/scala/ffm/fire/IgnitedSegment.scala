package ffm.fire

import ffm.geometry.Coord

/** An ignited canopy segment for a given time step. */
case class IgnitedSegment(timeStep: Int, start: Coord, end: Coord) {
  val length = start.distanceTo(end)
  require (length > 0.0, "Ignited segment length must be greater than 0")

  /**
   * Override equals so that the start and end coordinates are compared
   * robustly.
   */
  override def equals(other: Any) = {
    other match {
      case that: ffm.fire.IgnitedSegment =>
        that.canEqual(IgnitedSegment.this) &&
          timeStep == that.timeStep &&
          start.almostEq(that.start) &&
          end.almostEq(that.end)

      case _ => false
    }
  }

}
