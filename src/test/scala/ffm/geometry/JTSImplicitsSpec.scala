package ffm.geometry

import org.scalatest.FlatSpec
import org.scalatest.Matchers

import com.vividsolutions.jts.{ geom => JTS }

class JTSImplicitsSpec extends FlatSpec with Matchers {

  import JTSImplicits._
    
  val x = 1.0
  val y = -1.0

  "A JTS Coordinate" should "be converted to a Coord" in {
    val jtsCoord = new JTS.Coordinate(x, y)

    // this call requires an implicit conversion from JTS Coordinate to Coord
    testCoord(jtsCoord, x, y) should be(true)
  }

  "A Coord" should "be converted to a JTS Coordinate" in {
    val coord = Coord(x, y)
    
    // this call requires an implicit conversion from Coord to JTS Coordinate
    testJTSCoordinate(coord, x, y)
  }
  
  "An Array[Coord]" should "be converted to an Array[JTS.Coordinate]" in {
    val xys = Array((1, -2), (-3, 4), (5, 6), (-7, -8), (0, 0)) map (xy => (xy._1.toDouble, xy._2.toDouble))
    val coords = for ((x, y) <- xys) yield Coord(x, y)
    
    // this call requires the implicit conversion
    testArrayJTSCoordinate(coords, xys)
  }
  
  
  /*
   * Fixtures
   */
  
  def testCoord(coord: Coord, x: Double, y: Double): Boolean =
    coord.x === x && coord.y === y

  def testJTSCoordinate(c: JTS.Coordinate, x: Double, y: Double): Boolean =
    c.x === x && c.y === y
    
  def testArrayJTSCoordinate(cs: Array[JTS.Coordinate], xys: Iterable[(Double, Double)]): Boolean =
    cs.size === (xys.size) &&
    {
      val pairs = cs.zip(xys)
      !pairs.exists { case (c, (x, y)) => !(c.x === x && c.y === y) }
    }
}
