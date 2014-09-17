package ffm.fire

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar

import ffm.forest.{Site, Species, Stratum, StratumLevel}
import ffm.geometry.{Coord, Segment}

class IgnitionResultSpec extends FlatSpec with Matchers with MockitoSugar {

  val species = mock[Species]
  val stratumLevel = StratumLevel.Canopy
  val site = mock[Site]
  
  val initResult = IgnitionResult(species, stratumLevel, site)
  
  val c0 = Coord.Origin 
  val c1 = Coord(1, 1)
  val c2 = Coord(2, 2)
  
  "A newly created IgnitionResult" should "have no ignition time" in {
    initResult.ignitionTime should be (None)
  }
  
  it should "have no segments" in {
    initResult.segments.isEmpty should be (true)
  }
  
  it should "return false for hasIgnition" in {
    initResult.hasIgnition should be (false)
  }
  
  
  "An IgnitionResult" should "take the time of the first added segment as ignition time" in {
    val updatedResult = initResult.withSegment(3, c0, c1)
    updatedResult.ignitionTime should be (Some(3))
  }
  
  it should "return true for hasIgnition once a segment has been added" in {
    val updatedResult = initResult.withSegment(3, c0, c1)
    updatedResult.hasIgnition should be (true)
  }
  
  it should "throw an error on adding a segment with a time <= the previous segment" in {
    intercept[Error] {
      initResult.withSegment(1, c0, c1).withSegment(1, c1, c2)
    }
  }
  
}