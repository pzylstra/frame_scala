package ffm.geometry

import com.vividsolutions.jts.algorithm.{Angle, CGAlgorithms}
import com.vividsolutions.jts.{geom => JTS}

import ffm.numerics.Numerics
import JTSImplicits._

/**
 * Represents an infinite line defined by a representative coordinate
 * and an angle.
 */
class Line private(c: Coord, theta: Double) {
  
  val anchor = c
  
  /** The line angle normalized to (-Pi, Pi]. */
  val angle = Angle.normalize(theta)
  
  // arbitrary second point on line
  private val secondCoord = {
    val d = 1000.0
    anchor.toOffset(math.cos(angle) * d, math.sin(angle) * d)
  }
  
  /** True if this line is vertical or almost so.  */
  val isVertical = Numerics.almostEq(anchor.x, secondCoord.x)
  
  /** True if this line is horizontal or almost so.  */
  val isHorizontal = Numerics.almostEq(anchor.y, secondCoord.y)
  
  
  /**
   * Attempts to find a coordinate which lies on this line and forms the
   * origin of a Ray passing through targetPoint with the given angle.
   * 
   * The errorOnFail argument controls the behaviour when it is not possible
   * to find an origin: 
   * - if false (default), then None is returned
   * - if true, an error is thrown
   */
  def originOnLine(targetPoint: Coord, angle: Double, errorOnFail: Boolean = false): Option[Coord] = {
    if (intersects(targetPoint)) {
      // this line passes through the target point
      Some(targetPoint)
    }
    else {
      val ray = Ray(targetPoint, angle + math.Pi)
      val result = this.intersection(ray)
      
      if (errorOnFail && result.isEmpty)
        throw new Error(f"Failed to find origin for angle=${angle}%.4f and target point $targetPoint")
      else
        result
    }
  }
  
  /**
   * Finds the intersection point with another Line. If the two lines are 
   * parallel (including identical) then None is returned.
   */
  def intersection(other: Line): Option[Coord] = {
    // We use JTS classes for this operation
    val thisSeg = new JTS.LineSegment(anchor, secondCoord)
    val otherSeg = new JTS.LineSegment(other.anchor, other.secondCoord)
    val res = thisSeg.lineIntersection(otherSeg)
    
    if (res == null) None else Some(res)
  }

  /**
   * Finds the intersection point with a ray.
   * 
   * If the ray is parallel to, or pointing away from, this line then
   * None is returned.
   * 
   * The calculation does a line intersection and then checks that the angle
   * from the ray origin to the intersection coordinate matches the angle of 
   * the ray.
   */
  def intersection(ray: Ray): Option[Coord] = {
    intersection(Line(ray.origin, ray.angle)) match {
      case None => None
      case Some(coord) =>
        val theta = ray.origin.angleTo(coord)
        val diff = Angle.diff(theta, ray.angle)
        if ( Numerics.almostZero(diff)) Some(coord)
        else None
    }
  }
  
  /**
   * Tests if this Line passes through the given coordinate.
   */
  def intersects(c: Coord): Boolean =
    if (anchor.almostEq(c)) true
    else if (isVertical) Numerics.almostEq(c.x, anchor.x)
    else if (isHorizontal) Numerics.almostEq(c.y, anchor.y)  
    else Numerics.almostZero( distanceTo(c) )
  
  def distanceTo(c: Coord): Double =
    CGAlgorithms.distancePointLinePerpendicular(c, anchor, secondCoord)
   
}

/**
 * Companion object with factory methods to create Lines.
 */
object Line {
  /** Creates a new Line. */
  def apply(coord: Coord, angle: Double): Line =
    new Line(coord, angle)
  
  /** Creates a new vertical line. */
  def vertical(coord: Coord): Line =
    new Line(coord, math.Pi / 2)
  
  /** Creates a new horizontal line. */
  def horizontal(coord: Coord): Line =
    new Line(coord, 0.0)
  
}