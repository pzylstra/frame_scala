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
  when (context.speciesComponent) thenReturn spComp
  
  def newBuilder = IgnitionPathBuilder(IgnitionRunType.PlantRun, context, Coord.Origin)
  
  val c0 = Coord.Origin 
  val c1 = Coord(1, 1)
  val c2 = Coord(2, 2)
  
  "An IgnitionPathBuilder" should "initially have no segments" in {
    newBuilder.segments.isEmpty should be (true)
  }
  
  it should "initially return false for hasIgnition" in {
    newBuilder.hasIgnition should be (false)
  }
  
  it should "throw a NoSuchElementException for ignitionTime before ignition has occurred" in {
    intercept [NoSuchElementException] {
      newBuilder.ignitionTime
    }
  }
  
  it should "throw a NoSuchElementException for lastTimeStep before ignition has occurred" in {
    intercept [NoSuchElementException] {
      newBuilder.lastTimeStep
    }
  }
  
  it should "take the time of the first added segment as ignition time" in {
    val builder = newBuilder
    builder.addSegment(3, c0, c1)
    builder.ignitionTime should be (3)
  }
  
  it should "return true for hasIgnition once a segment has been added" in {
    val builder = newBuilder
    builder.addSegment(3, c0, c1)
    builder.hasIgnition should be (true)
  }
  
  it should "correctly add segments for consecutive time steps" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, c1)
    builder.addSegment(time + 1, c0, c1)
    builder.addSegment(time + 2, c1, c2)
    
    val expected = List(
      new IgnitedSegment(time, c0, c1),    
      new IgnitedSegment(time + 1, c0, c1),    
      new IgnitedSegment(time + 2, c1, c2)  
    )
    
    builder.segments should contain theSameElementsInOrderAs expected
  }
  
  it should "throw an error on adding a segment with a time <= the previous segment" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, c1)

    intercept[IllegalArgumentException] {
      builder.addSegment(time, c1, c2)
    }
    
    intercept[IllegalArgumentException] {
      builder.addSegment(time - 1, c1, c2)
    }
  }
  
  it should "throw an error on adding a segment with a time > previous time + 1" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, c1)

    intercept[IllegalArgumentException] {
      builder.addSegment(time + 2, c1, c2)
    }
  }
  
  it should "return an instance of IgnitionPath with the correct data" in {
    val builder = newBuilder
    val time = 1
    builder.addSegment(time, c0, c1)
    builder.addSegment(time + 1, c0, c1)
    builder.addSegment(time + 2, c1, c2)
    
    val expected = List(
      new IgnitedSegment(time, c0, c1),    
      new IgnitedSegment(time + 1, c0, c1),    
      new IgnitedSegment(time + 2, c1, c2)  
    )
    
    val path: IgnitionPath = builder.toIgnitionPath
  
    path.segments should contain theSameElementsInOrderAs expected  
    path.hasIgnition should be (true)
    path.ignitionTime should be(time)
  }
  
}