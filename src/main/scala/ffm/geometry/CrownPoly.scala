package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}
import ffm.numerics.Numerics

class CrownPoly private (jtsPoly: JTS.Polygon) {

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
    val axis = centroid.x
    val jtsCoords = jtsPoly.getExteriorRing().getCoordinates()
    
    var sum = 0.0
    for (Array(c0, c1) <- jtsCoords.sliding(2)) {
      if (Numerics.geq(c0.x, axis) && Numerics.geq(c1.x, axis)) {
        val r1 = c0.x - axis
        val r2 = c1.x - axis
        sum += (r1*r1 + r1*r2 + r2*r2) * (c0.y - c1.y)
      }
    }
    
    sum * math.Pi / 3.0
  }
      
}

object CrownPoly {
  private val factory = new JTS.GeometryFactory

  /**
   * Creates a new CrownPoly object from the given height values and width.
   */
  def apply(hc: Double, he: Double, ht: Double, hp: Double, w: Double): CrownPoly = {
    require(hp > hc, s"Invalid dimensions: hp ($hp) should be greater than hc ($hc)")
    require(ht >= he, s"Invalid dimensions: ht ($ht) should be greater than he ($he)")
    require(w > 0.0, s"Invalid dimensions: w ($w) should be positive")
    
    // set up x,y pairs for polygon coords
    val xys = Vector( (0.0, hc), (w/2, he), (w/2, ht), (0.0, hp), (-w/2, ht), (-w/2, he), (0.0, hc) )
    
    // transform to an array of JTS Coordinates and create JTS Polygon
    val coords = (xys map { case (x, y) => new JTS.Coordinate(x, y) }).toArray
    val jtsPoly = factory.createPolygon(coords)
    
    // ensure the polygon is in normal form
    jtsPoly.normalize()
    
    new CrownPoly(jtsPoly)
  }
}