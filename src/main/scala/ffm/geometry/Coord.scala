package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}

/**
 * An immutable wrapper around the JTS Coordinate class.
 */
class Coord(val x: Double, val y: Double) {
  
  private val jtsCoord: JTS.Coordinate = new JTS.Coordinate(x, y)
  
  def this(c: JTS.Coordinate) = this(c.x, c.y)
  
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
   * Finds the absolute distance between this Coord and another.
   */
  def distanceTo(other: Coord): Double = 
    jtsCoord.distance(other.jtsCoord) 
    
  override def toString =
    f"ImmutableCoordinate(${x}%.4f, ${y}%.4f)"
}

object Coord {
  def apply(x: Double, y: Double) =
    new Coord(x, y)
  
  def apply(jtsCoord: JTS.Coordinate) =
    new Coord(jtsCoord.x, jtsCoord.y)
}

