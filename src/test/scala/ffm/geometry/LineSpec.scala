package ffm.geometry

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues

class LineSpec extends FlatSpec with Matchers with OptionValues {

  val anchor = Coord.Origin
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
    val c = anchor.toOffset(bigDist, bigDist)
    line45.intersects(c) should be(true)
  }

  it should "intersect a distant point in the negative direction" in {
    val c = anchor.toOffset(-bigDist, -bigDist)
    line45.intersects(c) should be(true)
  }

  it should "not intersect a point off the line" in {
    line45.intersects(anchor.toOffset(1.0, 0.0)) should be(false)
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
    line45.distanceTo(anchor.toOffset(1.0, 0.0)) should be (1 / math.sqrt(2.0) +- Tol)
  }
  
  it should "find the correct intersection point with a non-parallel line" in {
    val other = Line.vertical(Coord(10.0, 0.0))
    line45.intersection(other).value.almostEq(Coord(10.0, 10.0)) should be (true)
  }
  
  it should "return None for the intersection point with itself" in {
    line45.intersection(line45) should be (None)
  }
  
  it should "not intersect a distinct parallel line" in {
    val other = Line(anchor.toOffset(0.0, 1.0), line45.angle)
    line45.intersection(other) should be (None)
  }
  
  it should "find the correct intersection with a horizontal ray" in {
    // ray along x axis in negative direction
    val ray = Ray(Coord(10, 0), -math.Pi)
    val expectedCoord = Coord.Origin 
    
    line45.intersection(ray).value.almostEq(expectedCoord) should be (true)
  }
  
  it should "not find an intersection with a ray that does not cross it when strict is true" in {
    val ray = Ray(line45.anchor.toOffset(0, 1), math.Pi / 2)
    line45.intersection(ray) should be (None)
  }
  
  
  /////////////////////////////////////////////////////////////////////////////
  // tests of method originOnLine
  
  "method originOnLine" should "return None by default when no origin is found" in {
    val below = Coord(0, -1)
    line45.originOnLine(below, math.Pi / 2) should be (None)
  }
  
  it should "throw an error when no origin is found and errorOnFail is true" in {
    val below = Coord(0, -1)
    intercept[Error] {
      line45.originOnLine(below, math.Pi / 2, errorOnFail = true)
    }
  }
  
  it should "find the correct origin for a point above the line" in {
    val c = Coord(10, 20)
    val theta = math.Pi / 3
    
    val b = c.y - math.tan(theta) * c.x
    val expectedX = b / (1 - math.tan(theta))
    val expectedOrigin = Coord(expectedX, expectedX)
    
    line45.originOnLine(c, theta) match {
      case None => fail("failed to find origin")
      case Some(coord) =>
        coord.almostEq(expectedOrigin) should be (true)
    }
  }
  
  it should "find the correct origin for a point below the line" in {
    val c = Coord(10, 0)
    val theta = -math.Pi / 3
    
    val b = c.y - math.tan(theta) * c.x
    val expectedX = b / (1 - math.tan(theta))
    val expectedOrigin = Coord(expectedX, expectedX)
    
    line45.originOnLine(c, theta) match {
      case None => fail("failed to find origin")
      case Some(coord) =>
        coord.almostEq(expectedOrigin) should be (true)
    }
  }
  
  it should "return the target point when that point lies on the line" in {
    for (i <- 1 to 100) {
      // random coord on line45
      val x = math.random
      val targetPoint = Coord(x, x)
      
      // angle should have no effect
      val angle = math.random * 2 * math.Pi
      
      line45.originOnLine(targetPoint, angle) match {
        case None => fail("failed to find intersection")
        case Some(coord) =>
          coord.almostEq(targetPoint) should be (true)
      }
    }
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