package ffm.geometry

class Ray(val origin: Coord, val angle: Double) {

}

object Ray {
  def apply(origin: Coord, angle: Double) = new Ray(origin, angle)
}