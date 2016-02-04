package ffm.geometry

import JTSImplicits._
import ffm.numerics.Numerics
import com.vividsolutions.jts.{ geom => JTS }
import com.vividsolutions.jts.geom.TopologyException
  

/** 
 * Holds input parameters for a hexagonal crown polygon. 
 *  
 * The parameters are used to form a hexagon with the following vertices
 * arranged clockwise, from the lower centre vertex: (0, hc) -> (-w/2, he) -> 
 * (-w/2, ht) -> (0, hp) -> (w/2, ht) -> (w/2, he)
 *
 * @param hc y ordinate of lower centre vertex
 * @param he y ordinate of lower side vertices
 * @param ht y ordinate of upper sider vertices
 * @param hp y ordinate of upper centre vertex
 * @param w crown width.
 */
case class CrownParams(hc: Double, he: Double, ht: Double, hp: Double, w: Double) {
  require(hp > hc, s"Invalid dimensions: hp ($hp) should be greater than hc ($hc)")
  require(ht >= he, s"Invalid dimensions: ht ($ht) should not be less than he ($he)")
  require(w > 0.0, s"Invalid dimensions: width ($w) should be positive")  
}


/**
 * A polygon representing a plant crown.
 *
 * Uses JTS classes for the geometry internals but all of the mutable
 * JTS objects are hidden from clients.
 * 
 * Create instances with the companion CrownPoly object.
 */
class CrownPoly private (jtsPoly: JTS.Polygon, params: Option[CrownParams]) {

  // bounding rectangle of the crown aligned with coordinate axes
  private val env = jtsPoly.getEnvelopeInternal()
  
  /** The input parameters (hc, he, ht, hp, w) used to create this crown
   *  polygon. These are optional, as a crown can also be created from
   *  arbitrary vertices.
   */
  val inputParams: Option[CrownParams] = params;

  val width: Double = env.getWidth
  val height: Double = env.getHeight

  val top: Double = env.getMaxY
  val bottom: Double = env.getMinY
  val left: Double = env.getMinX
  val right: Double = env.getMaxX

  val centroid: Coord = jtsPoly.getCentroid().getCoordinate()

  val area: Double = jtsPoly.getArea
  
  val lowerLeft: Coord =
    vertices.filter(c => Numerics.Distance.almostEq(c.x, left)).sortBy(_.y).head
  
  val lowerRight: Coord =
    vertices.filter(c => Numerics.Distance.almostEq(c.x, right)).sortBy(_.y).head

  /**
   * Volume of revolution (assumes bilateral symmetry of polygon).
   */
  val volume: Double = {
    val midx = centroid.x
    var sum = 0.0
    for (Segment(c0, c1) <- segments) {
      if (Numerics.Distance.geq(c0.x, midx) && Numerics.Distance.geq(c1.x, midx)) {
        val r1 = c0.x - midx
        val r2 = c1.x - midx
        sum += (r1 * r1 + r1 * r2 + r2 * r2) * (c0.y - c1.y)
      }
    }

    sum * math.Pi / 3.0
  }
  
  def vertices: IndexedSeq[Coord] = {
    val coords: Array[Coord] = jtsPoly.getExteriorRing().getCoordinates()
    coords.toVector
  }
  
  /**
   * Iterator for the Segments making up this crown polygon.
   */
  def segments: Iterator[Segment] =
    for (Array(jtsC0, jtsC1) <- jtsPoly.getExteriorRing().getCoordinates().sliding(2))
      yield Segment(jtsC0, jtsC1)

  /**
   * Finds the intersection between this crown polygon and a Ray.
   */
  def intersection(ray: Ray): Option[Segment] = {
    // A point on the ray 1000m from the origin (ie. further than any
    // plant crown extent)
    val outsideCoord = ray.atDistance(1000)

    val coords = Array(ray.origin, outsideCoord)

    val jtsLine = JTSGeometryFactory.createLineString(coords)

    try {
      val res: JTS.Geometry = jtsPoly.intersection(jtsLine)

      if (res.isEmpty()) None
      else if (Numerics.Distance.almostZero( res.getLength )) None
      else Some({
        val jtsCoords = res.getCoordinates()
        Segment(jtsCoords.head, jtsCoords.last)
      })
    } catch {
      case ex: TopologyException =>
        // TODO - Needs checking. This happens for a small number 
        // of input param files.
        val exc = ex.getCoordinate()
        None
    }
  }

  /**
   * Find the point in the base of this crown polygon with the given X ordinate.
   *
   * If x is beyond the edges of the polygon it is clamped to the nearest edge.
   */
  def pointInBase(x: Double): Coord = {
    if (Numerics.Distance.leq(x, left)) lowerLeft
    else if (Numerics.Distance.geq(x, right)) lowerRight
    else {
      val verticalRay = Ray(Coord(x, bottom - 1.0), angle = math.Pi / 2)
   
      intersection(verticalRay) match {
        case Some(seg) => Coord(x, seg.start.y)
        case None => throw new Error(s"No base point for x=$x in crown width=$width")
      }
    }
  }

  /**
   * Tests if a coordinate lies within the polygon.
   */
  def contains(c: Coord): Boolean = {
    val p = JTSGeometryFactory.createPoint(c)
    jtsPoly.contains(p)
  }

  override def toString = s"CrownPoly(width=$width)"
}


object CrownPoly {
  /**
   * Creates a new six-sided crown polygon from the given height and width parameters.
   */
  def apply(hc: Double, he: Double, ht: Double, hp: Double, w: Double): CrownPoly = {
    // Store parameters. This will also run validity checks on the values.
    val params = CrownParams(hc, he, ht, hp, w)

    // set up x,y pairs for polygon coords
    val w2 = w / 2
    val xys = Array((0.0, hc), (w2, he), (w2, ht), (0.0, hp), (-w2, ht), (-w2, he), (0.0, hc))

    // create Coords (implicitly JTS Coordinates) and build the polygon
    val coords = for ((x, y) <- xys) yield Coord(x, y)
    val poly = JTSGeometryFactory.createPolygon(coords)

    // ensure the polygon is in normal form, then return it
    poly.normalize()

    new CrownPoly(poly, Some(params))
  }
  
  /**
   * Creates a new arbitrary polygon from vertex coordinates.
   */
  def apply(coords: IndexedSeq[Coord]): CrownPoly = {
    require(coords.length > 2)
    
    // ensure the coords define a closed ring
    val closedCoords = 
      if (coords.head.almostEq(coords.last)) coords
      else coords :+ coords.head
      
    val poly = JTSGeometryFactory.createPolygon(closedCoords)
    if (!poly.isValid) throw new Error("Invalid crown polygon from coords: " + coords.mkString(", "))
    
    poly.normalize()
    new CrownPoly(poly, None)
    
  }
}