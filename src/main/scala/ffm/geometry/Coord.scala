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
  def toOffset(dx: Double, dy: Double) =
    Coord(x + dx, y + dy)
    
  /**
   * Returns a new Coord at the given angle (radians) and distance 
   * from this Coord.
   */
  def toBearing(angle: Double, distance: Double) =
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
    
  /**
   * Finds the ordinate offsets between this Coord and another.
   * 
   * Returns a tuple (other.x - x, other.y - y)
   */
  def offsetTo(other: Coord): (Double, Double) =
    (other.x - x, other.y - y)
    
  /**
   * Tests if this Coord is close to another by comparing X and Y ordinates
   * separately against a thresholds defined by the implicit tol XYTolerance argument.
   */
  def closeTo(other: Coord)(implicit tol: XYTolerance): Boolean =
    (x - other.x).abs <= tol.xtol && (y - other.y).abs <= tol.ytol 

  /**
   * Tests if this Coord is at the same position as another, or almost so.
   * 
   * A short-cut for closeTo(other)(XYTolerance.Tiny)
   */
  def almostEq(other: Coord): Boolean =
    closeTo(other)(XYTolerance.Tiny)
    
  /**
   * Tests if this Coord is at a different position to another.
   * 
   * Syntactic sugar for !closeTo(other)
   */
  def distinctFrom(other: Coord): Boolean =
    !almostEq(other)
    
  /**
   * String representation.
   */
  override def toString =
    f"Coord(${x}%.8f, ${y}%.8f)"
}

object Coord {
  /** The origin coordinate (0, 0). */
  val Origin = Coord(0, 0)
  
}

