package ffm.fire

import ffm.BasicSpec
import ffm.geometry.Coord
import ffm.numerics.Numerics
import org.scalatest.prop.PropertyChecks

class IgnitedSegmentSpec extends BasicSpec with PropertyChecks {  
  
  "An IgnitedSegment" should "calculate the correct length" in {
    val p0 = Coord.Origin
    val p1 = p0.toOffset(3.0, 4.0)
    val seg = IgnitedSegment(1, p0, p1, 1.0)
    seg.length should be (5.0 +- Numerics.DefaultTolerance)
  }
  
  it should "return its end points in the correct order" in {
    val p0 = Coord(1, 2)
    val p1 = Coord(2, 3)
    val seg = IgnitedSegment(1, p0, p1, 1.0)
    
    seg.start.almostEq(p0) should be (true)
    seg.end.almostEq(p1) should be (true)
  }
  
  it should "not allow end points closer than the distance tolerance" in {
    val p0 = Coord.Origin
    val p1 = p0.toOffset(0.0, Numerics.DistanceTolerance / 2)
    
    intercept[IllegalArgumentException] {
      IgnitedSegment(1, p0, p1, 1.0)
    }
  }
  
  it should "throw an error if the flame length is zero" in {
    val p0 = Coord(1, 2)
    val p1 = p0.toOffset(2, 3)
    
    intercept[IllegalArgumentException] {
      IgnitedSegment(1, p0, p1, 0.0)
    }
  }
  
  it should "throw an error if the flame length is negative" in {
    val p0 = Coord(1, 2)
    val p1 = p0.toOffset(2, 3)
    
    intercept[IllegalArgumentException] {
      IgnitedSegment(1, p0, p1, -1.0)
    }
  }
}