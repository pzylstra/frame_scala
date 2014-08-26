package ffm.geometry

import org.scalatest.FlatSpec
import org.scalatest.Matchers

class LineSpec extends FlatSpec with Matchers {

  val anchor = Coord(0, 0)
  val line45 = Line(anchor, math.Pi / 4)
  val bigDist = 1000.0
  
  val Tol = 1.0e-8

  "An angled line" should "return false for isVertical and isHorizontal" in {
    line45.isHorizontal should be(false)
    line45.isVertical should be(false)
  }

  "A line" should "intersect its anchor point" in {
    line45.intersects(anchor) should be(true)
  }

  it should "intersect a distant point in the postive direction" in {
    val c = anchor.offset(bigDist, bigDist)
    line45.intersects(c) should be(true)
  }

  it should "intersect a distant point in the negative direction" in {
    val c = anchor.offset(-bigDist, -bigDist)
    line45.intersects(c) should be(true)
  }

  it should "not intersect a point off the line" in {
    line45.intersects(anchor.offset(1.0, 0.0)) should be(false)
  }
  
  it should "report an angle normalized to (-Pi, Pi]" in {
    def f(inAngle: Double, outAngle: Double) {
      Line(anchor, inAngle).angle should be (outAngle)
    }
    
    f(-math.Pi, math.Pi)
    f(2 * math.Pi,  0.0)
    f(-2 * math.Pi, 0.0)
    f(-math.Pi, math.Pi)
    f(3 * math.Pi / 2, -math.Pi / 2)
    f(-3 * math.Pi / 2, math.Pi / 2)
  }
  
  it should "have zero distance to its anchor" in {
    line45.distanceTo(anchor) should be (0.0 +- Tol)
  }
  
  it should "calculate the correct distance to an off-line point" in {
    line45.distanceTo(anchor.offset(1.0, 0.0)) should be (1 / math.sqrt(2.0) +- Tol)
  }
  
  it should "find the correct intersection point with a non-parallel line" in {
    val other = Line.vertical(Coord(10.0, 0.0))
    line45.intersection(other) match {
      case None => fail("intersection not found")
      case Some(cx) => cx.closeTo(Coord(10.0, 10.0))(XYTolerance.default) should be (true)
    }
  }
  
  it should "return None for the intersection point with itself" in {
    line45.intersection(line45) should be (None)
  }
  
  it should "not intersect a distinct parallel line" in {
    val other = Line(anchor.offset(0.0, 1.0), line45.angle)
    line45.intersection(other) should be (None)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Vertical line tests

  "A vertical line" should "return true for isVertical" in {
    Line.vertical(anchor).isVertical should be(true)
  }

  it should "intersect a distant point" in {
    val line = Line.vertical(anchor)
    line.intersects(Coord(anchor.x, bigDist)) should be(true)
    line.intersects(Coord(anchor.x, -bigDist)) should be(true)
  }

  /////////////////////////////////////////////////////////////////////////////
  // Horizontal line tests

  "A horizontal line" should "return true for isHorizontal" in {
    Line.horizontal(anchor).isHorizontal should be(true)
  }

  it should "intersect a distant point" in {
    val line = Line.horizontal(anchor)
    line.intersects(Coord(bigDist, anchor.y)) should be(true)
    line.intersects(Coord(-bigDist, anchor.y)) should be(true)
  }
}