package ffm.fire

import ffm.geometry.Coord

class IgnitionPathBuilderSpec extends IgnitionPathTestBase {
  
  val FlameLen = 1.0
  
  "An IgnitionPathBuilder" should "initially have no segments" in {
    newBuilder().segments.isEmpty should be (true)
  }
  
  it should "initially return false for hasIgnition" in {
    newBuilder().hasIgnition should be (false)
  }
  
  it should "throw an UnsupportedOperationException for ignitionTimeStep before ignition has occurred" in {
    intercept [UnsupportedOperationException] {
      newBuilder().ignitionTimeStep
    }
  }
  
  it should "throw an execption if the first segment start point is different to the builder initial point" in {
    intercept [IllegalArgumentException] {
      val init = Coord(1.0, 2.0)
      val builder = IgnitionPathBuilder(context, spComp, init)
      
      val other = Coord(3.0, 4.0)
      builder.addSegment(1, other, other.toOffset(1.0, 1.0), FlameLen)
    }
  }
  
  it should "take the time of the first added segment as ignition time" in {
    val builder = newBuilder()
    builder.addSegment(3, c0, atX(1.0), FlameLen)
    builder.ignitionTimeStep should be (3)
  }
  
  it should "return true for hasIgnition once a segment has been added" in {
    val builder = newBuilder()
    builder.addSegment(3, c0, atX(1.0), FlameLen)
    builder.hasIgnition should be (true)
  }
  
  it should "correctly add segments for consecutive time steps" in {
    val builder = newBuilder()
    val time = 1

    val segments = List(
      new IgnitedSegment(time, c0, atX(1.0), FlameLen),    
      new IgnitedSegment(time + 1, c0, atX(1.0), FlameLen),    
      new IgnitedSegment(time + 2, atX(1.0), atX(2.0), FlameLen)  
    )
    
    segments foreach (builder.addSegment(_))
    
    builder.segments should contain theSameElementsInOrderAs segments
  }
  
  it should "throw an error on adding a segment with a time <= the previous segment" in {
    val builder = newBuilder()
    val time = 1
    builder.addSegment(time, c0, atX(1.0), FlameLen)

    intercept[IllegalArgumentException] {
      builder.addSegment(time, atX(1.0), atX(2.0), FlameLen)
    }
    
    intercept[IllegalArgumentException] {
      builder.addSegment(time - 1, atX(1.0), atX(2.0), FlameLen)
    }
  }
  
  it should "throw an error on adding a segment with a time > previous time + 1" in {
    val builder = newBuilder()
    val time = 1
    builder.addSegment(time, c0, atX(1.0), FlameLen)

    intercept[IllegalArgumentException] {
      builder.addSegment(time + 2, atX(1.0), atX(2.0), FlameLen)
    }
  }
  
  it should "return an instance of IgnitionPath with the correct data" in {
    val builder = newBuilder()
    val time = 1
    
    val segments = List(
      new IgnitedSegment(time, c0, atX(1.0), FlameLen),    
      new IgnitedSegment(time + 1, c0, atX(1.0), FlameLen),    
      new IgnitedSegment(time + 2, atX(1.0), atX(2.0), FlameLen)  
    )
    
    segments foreach (builder.addSegment(_))
    
    val path: IgnitionPath = builder.toIgnitionPath
    
    path.segments should contain theSameElementsInOrderAs segments  
    path.hasIgnition should be (true)
    path.ignitionTime should be(time)
  }
  
}