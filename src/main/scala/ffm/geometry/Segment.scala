package ffm.geometry

case class Segment(start: Coord, end: Coord) {
  val length = start.distanceTo(end)
}