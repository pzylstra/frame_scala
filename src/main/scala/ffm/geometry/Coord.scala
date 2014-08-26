package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}

/**
 * An immutable wrapper around the JTS Coordinate class.
 */
case class Coord(val x: Double, val y: Double) {
  
  private val jtsCoord: JTS.Coordinate = new JTS.Coordinate(x, y)
  
  /**
   * Returns a new Coord offset from this Coord by dx, dy.
   */
  def offset(dx: Double, dy: Double) =
    Coord(x + dx, y + dy)
    
  /**
   * Returns a new Coord at the given angle (radians) and distance 
   * from this Coord.
   */
  def bearing(angle: Double, distance: Double) =
    Coord(x + distance * math.cos(angle), y + distance * math.sin(angle))
    
  /**
   * Finds the angle to another Coord.
   */
  def angleTo(other: Coord): Double =
    math.atan2(other.y - y, other.x - x)
    
  /**
   * Finds the absolute distance between this Coord and another.
   */
  def distanceTo(other: Coord): Double = 
    jtsCoord.distance(other.jtsCoord) 
    
  def closeTo(other: Coord)(implicit tol: XYTolerance): Boolean =
    (x - other.x).abs <= tol.xtol && (y - other.y).abs <= tol.ytol 
    
  override def toString =
    f"ImmutableCoordinate(${x}%.4f, ${y}%.4f)"
}

object Coord {
  def apply(jtsCoord: JTS.Coordinate) =
    new Coord(jtsCoord.x, jtsCoord.y)
}

