package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}
import ffm.numerics.Numerics

/**
 * A polygon with six vertices representing a plant crown.
 *
 * Uses JTS classes for the geometry internals but all of the mutable
 * JTS stuff is hidden from the clients of this class.
 */
class CrownPoly private (val hc: Double, val he: Double, val ht: Double, val hp: Double, w: Double) {
  require(hp > hc, s"Invalid dimensions: hp ($hp) should be greater than hc ($hc)")
  require(ht >= he, s"Invalid dimensions: ht ($ht) should be greater than he ($he)")
  require(w > 0.0, s"Invalid dimensions: width ($w) should be positive")

  import ffm.geometry.JTSImplicits._

  // JTS Polygon to which we delegate geometric computations
  private val jtsPoly = createPoly()

  // bounding rectangle of the crown aligned with coordinate axes
  private val env = jtsPoly.getEnvelopeInternal()
  
  val width: Double = w
  val height: Double = env.getHeight

  val top: Double = env.getMaxY
  val bottom: Double = env.getMinY
  val left: Double = env.getMinX
  val right: Double = env.getMaxX
  
  val centroid: Coord = jtsPoly.getCentroid().getCoordinate()
  
  val area: Double = jtsPoly.getArea
  
  /**
   * Volume of revolution (assumes bilateral symmetry of polygon).
   */
  val volume: Double = { 
    val midx = centroid.x
    var sum = 0.0
    for (Segment(c0, c1) <- segments) {
      if (Numerics.geq(c0.x, midx) && Numerics.geq(c1.x, midx)) {
        val r1 = c0.x - midx
        val r2 = c1.x - midx
        sum += (r1*r1 + r1*r2 + r2*r2) * (c0.y - c1.y)
      }
    }
    
    sum * math.Pi / 3.0
  }
      
  /**
   * Iterator for the LineSegments making up this crown polygon.
   */
  def segments: Iterator[Segment] =
    for (Array(jtsC0, jtsC1) <- jtsPoly.getExteriorRing().getCoordinates().sliding(2))
      yield Segment(jtsC0, jtsC1)
  
  /**
   * Finds the intersection between this crown polygon and a Ray.
   */
  def intersection(ray: Ray): Option[Segment] = {
    val c0 = ray.origin 
    
    // A point on the Ray outside the envelope of this polygon
    val c1 = ray.origin.toBearing(ray.angle, 2 * width.max(height))
    
    val coords = Array(c0, c1)
    val jtsLine = CrownPoly.factory.createLineString(coords)
    val res: JTS.Geometry = jtsPoly.intersection(jtsLine)
    
    if (res.isEmpty()) None
    else Some( {
      val jtsCoords = res.getCoordinates()
      Segment(jtsCoords.head, jtsCoords.last)
    } )
  }

  /**
   * Returns the The point in the base of the Poly with given x-value
  \param x
  \return The lowest (ie least y value) point in the Poly that has x-coordinate equal to x.

  If x is greater than right() then the function returns the lowest y-value with x == right(). Similarly if 
  x is less than left() then the function returns the least y-value with x == left().
*/
  def pointInBase(x: Double): Coord = {
    if (Numerics.leq(x, left))  // treat as left => vertex 'd'
      Coord(left, he)
    else if (Numerics.geq(x, right))  // treat as right => vertex 'e'
      Coord(right, he)
    else if (Numerics.almostZero(x))  // centre => vertex 'f'
      Coord(0, hc)
    else {  // intermediate x position => find point by intersection
      val verticalRay = Ray(Coord(x, bottom - 1.0), angle=math.Pi / 2)
      val crossingSegment = intersection(verticalRay).get
      Coord(x, crossingSegment.start.y)
    }
  }
  
  /**
   * Creates the underlying JTS Polygon object.
   */
  private def createPoly(): JTS.Polygon = {
    // set up x,y pairs for polygon coords
    val xys = Array( (0.0, hc), (w/2, he), (w/2, ht), (0.0, hp), (-w/2, ht), (-w/2, he), (0.0, hc) )
    
    // create Coords (implicitly JTS Coordinates) and build the polygon
    val coords = for ((x, y) <- xys) yield Coord(x, y)
    val poly = CrownPoly.factory.createPolygon(coords)
    
    // ensure the polygon is in normal form, then return it
    poly.normalize()
    poly
  }
}

object CrownPoly {
  private val factory = new JTS.GeometryFactory

  /**
   * Creates a new CrownPoly object from the given height values and width.
   */
  def apply(hc: Double, he: Double, ht: Double, hp: Double, w: Double): CrownPoly = {
    new CrownPoly(hc, he, ht, hp, w)
  }
}