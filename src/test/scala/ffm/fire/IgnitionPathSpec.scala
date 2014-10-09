package ffm.fire

import org.mockito.Mockito.when
import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.OptionValues
import org.scalatest.mock.MockitoSugar
import ffm.forest.{Site, Species, Stratum, StratumLevel}
import ffm.geometry.{Coord, Segment}
import ffm.forest.SpeciesComponent


class IgnitionPathSpec extends FlatSpec with Matchers with MockitoSugar with OptionValues {

  val species = mock[Species]
  val spComp = SpeciesComponent(species, 1.0)
  val stratumLevel = StratumLevel.Canopy
  val site = mock[Site]
  
  val context = mock[IgnitionContext]
  when (context.site) thenReturn site
  when (context.stratumLevel) thenReturn stratumLevel

  // This extends the IgntionPathBuilder with a method to 
  // add segments directly
  implicit class IgnitionPathBuilderEx(builder: IgnitionPathBuilder) {
    def addSegment(s: IgnitedSegment): Unit =
      builder.addSegment(s.timeStep, s.start, s.end)
  }

  def newBuilder = IgnitionPathBuilder(context, spComp, c0)
  
  val c0 = Coord.Origin 
  val pos = (d: Double) => c0.toOffset(d, 0.0)  
  
  
  "An IgnitionPathBuilder" should "initially have no segments" in {
    newBuilder.segments.isEmpty should be (true)
  }
  
  it should "initially return false for hasIgnition" in {
    newBuilder.hasIgnition should be (false)
  }
  
  it should "throw a NoSuchElementException for ignitionTimeStep before ignition has occurred" in {
    intercept [NoSuchElementException] {
      newBuilder.ignitionTimeStep
    }
  }
  
  it should "take the time of the first added segment as ignition time" in {
    val builder = newBuilder
    builder.addSegment(3, c0, pos(1.0))
    builder.ignitionTimeStep should be (3)
  }
  
  it should "return true for hasIgnition once a segment has been added" in {
    val builder = newBuilder
    builder.addSegment(3, c0, pos(1.0))
    builder.hasIgnition should be (true)
  }
  
  it should "correctly add segments for consecutive time steps" in {
    val builder = newBuilder
    val time = 1

    val segments = List(
      new IgnitedSegment(time, c0, pos(1.0)),    
      new IgnitedSegment(time + 1, c0, pos(1.0)),    
      new IgnitedSegment(time + 2, pos(1.0), pos(2.0))  
    )
    
    segments foreach (builder.addSegment(_))
    
    builder.segments should contain theSameElementsInOrderAs segments
  }
  
  it should "throw an error on adding a segment with a time <= the previous segment" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, pos(1.0))

    intercept[IllegalArgumentException] {
      builder.addSegment(time, pos(1.0), pos(2.0))
    }
    
    intercept[IllegalArgumentException] {
      builder.addSegment(time - 1, pos(1.0), pos(2.0))
    }
  }
  
  it should "throw an error on adding a segment with a time > previous time + 1" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, pos(1.0))

    intercept[IllegalArgumentException] {
      builder.addSegment(time + 2, pos(1.0), pos(2.0))
    }
  }
  
  it should "return an instance of IgnitionPath with the correct data" in {
    val builder = newBuilder
    val time = 1
    
    val segments = List(
      new IgnitedSegment(time, c0, pos(1.0)),    
      new IgnitedSegment(time + 1, c0, pos(1.0)),    
      new IgnitedSegment(time + 2, pos(1.0), pos(2.0))  
    )
    
    segments foreach (builder.addSegment(_))
    
    val path: IgnitionPath = builder.toIgnitionPath
    
    path.segments should contain theSameElementsInOrderAs segments  
    path.hasIgnition should be (true)
    path.ignitionTime should be(time)
  }
  
  it should "find the correct earliest time step with max segment length" in {
    val builder = newBuilder
    val time = 5
    
    builder.addSegment(time, c0, pos(0.1))
    builder.addSegment(time + 1, c0, pos(0.1))
    builder.addSegment(time + 2, pos(0.1), pos(1.0))  // earliest max length segment
    builder.addSegment(time + 3, pos(0.1), pos(1.0))
    builder.addSegment(time + 4, pos(1.0), pos(1.1))
    
    val path = builder.toIgnitionPath
    path.timeStepForMaxLength should be (time + 2)
  }
  
  it should "find the correct time from ignition to max segment length" in {
    val builder = newBuilder
    val igTime = 2
    val maxLenTime = 6

    (igTime until maxLenTime) foreach (t => builder.addSegment(t, c0, pos(0.1)))
    (maxLenTime to (maxLenTime + 2)) foreach (t => builder.addSegment(t, pos(0.1), pos(1.0)))
    
    val path = builder.toIgnitionPath
    path.timeFromIgnitionToMaxLength should be (maxLenTime - igTime)
  }
  
}