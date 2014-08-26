package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}
import ffm.numerics.Numerics

/**
 * A polygon with six vertices representing a plant crown.
 * 
 * Uses JTS classes for the geometry internals but all of the mutable
 * JTS stuff is hidden from the clients of this class.
 */
class CrownPoly private (jtsPoly: JTS.Polygon) {
  
  import ffm.geometry.JTSImplicits._

  // bounding rectangle of the crown aligned with coordinate axes
  private val env = jtsPoly.getEnvelopeInternal()
  
  val width: Double = env.getWidth
  val height: Double = env.getHeight

  val top: Double = env.getMaxY
  val bottom: Double = env.getMinY
  val left: Double = env.getMinX
  val right: Double = env.getMaxX
  
  val centroid: Coord = Coord(jtsPoly.getCentroid().getCoordinate())
  
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
    val c1 = ray.origin.bearing(ray.angle, 2 * width.max(height))
    
    val coords = Array(c0, c1)
    val jtsLine = CrownPoly.factory.createLineString(coords)
    val res: JTS.Geometry = jtsPoly.intersection(jtsLine)
    
    if (res.isEmpty()) None
    else Some( {
      val jtsCoords = res.getCoordinates()
      Segment(jtsCoords.head, jtsCoords.last)
    } )
  }
}

object CrownPoly {
  private val factory = new JTS.GeometryFactory

  import ffm.geometry.JTSImplicits._
  
  /**
   * Creates a new CrownPoly object from the given height values and width.
   */
  def apply(hc: Double, he: Double, ht: Double, hp: Double, w: Double): CrownPoly = {
    require(hp > hc, s"Invalid dimensions: hp ($hp) should be greater than hc ($hc)")
    require(ht >= he, s"Invalid dimensions: ht ($ht) should be greater than he ($he)")
    require(w > 0.0, s"Invalid dimensions: w ($w) should be positive")
    
    // set up x,y pairs for polygon coords
    val xys = Array( (0.0, hc), (w/2, he), (w/2, ht), (0.0, hp), (-w/2, ht), (-w/2, he), (0.0, hc) )
    
    // create Coords (implicitly JTS Coordinates) and build the polygon
    val coords = for ((x, y) <- xys) yield Coord(x, y)
    val jtsPoly = factory.createPolygon(coords)
    
    // ensure the polygon is in normal form
    jtsPoly.normalize()
    
    new CrownPoly(jtsPoly)
  }
}