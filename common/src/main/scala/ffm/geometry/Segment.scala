package ffm.geometry

/**
 * A line segment.
 * 
 * The input start and end coordinates are snapped to the geometry precision
 * grid, so values for start and end may differ from the input startPos and
 * endPos values.
 */
case class Segment(start: Coord, end: Coord) {
  val length = start.distanceTo(end)
}

