package ffm.fire

import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.prop.PropertyChecks

import ffm.BasicSpec
import ffm.geometry.Coord
import ffm.geometry.Ray

class FlameSpec extends BasicSpec with PropertyChecks with OptionValues {

  /*
   * Generators for ordinate values, Coords, angles etc.
   */
  val XLim = 10.0
  val YLim = 10.0
  
  val validXY: Gen[(Double, Double)] =
    for {
      x <- Gen.choose(-XLim, XLim)
      y <- Gen.choose(-YLim, YLim)
    } yield (x, y)
  
  val coords: Gen[Coord] = for ((x, y) <- validXY) yield Coord(x, y)
  
  val angles: Gen[Double] = Gen.choose(-math.Pi, math.Pi)
  
  val distances: Gen[Double] = Gen.choose(0.0, XLim)
  
  /*
   * General tolerance for +- comparisons
   */
  val Tol = ffm.numerics.Numerics.DefaultTol.value

  
  "A Flame" should "ensure that length > 0" in {
    intercept [IllegalArgumentException] {
      Flame(length=0.0, angle=1.0, origin=Coord.Origin, depthIgnited = 0.0, deltaTemperature=300.0)
    }
  }
  
  it should "ensure that depth ignited > 0" in {
    intercept [IllegalArgumentException] {
      Flame(length=0.1, angle=1.0, origin=Coord.Origin, depthIgnited = 0.0, deltaTemperature=300.0)
    }
  }
  
  it should "ensure that length > depth ignited" in {
    intercept [IllegalArgumentException] {
      Flame(length=0.1, angle=1.0, origin=Coord.Origin, depthIgnited = 0.2, deltaTemperature=300.0)
    }
  }
  
  it should "return the correct plume" in {
    forAll (coords, angles) { (origin, angle) =>
      val flame = Flame(0.5, angle, origin, 0.1, 300.0)

      val expectedPlume = Ray(origin, angle)
      
      flame.plume.almostEq(expectedPlume) should be (true)
    }
  }
  
  it should "create a Flame with a new origin" in {
    forAll (coords, coords) { (origin, newOrigin) => 
      val flame = Flame(0.5, 0.0, origin, 0.1, 300.0)
      val newFlame = flame.toOrigin(newOrigin)
      
      newFlame.origin.almostEq(newOrigin) should be (true)

      newFlame.flameLength should be (flame.flameLength)
      newFlame.angle should be (flame.angle)
      newFlame.depthIgnited should be (flame.depthIgnited)
      newFlame.deltaTemperature should be (flame.deltaTemperature)
    }
  }
  
  it should "return the flame temperature for distances <= depth ignited and lower temperatures otherwise" in {
    val depthIgnited = XLim / 2
    val flameLength = depthIgnited + 1.0
    val t = 300.0
    
    val flame = Flame(flameLength, 0.0, Coord.Origin, depthIgnited, t)
    
    forAll (distances) { d =>
      val dT = flame.plumeDeltaTemperature(d)
      if (d <= depthIgnited) dT should be (t)
      else dT should be < t
    }
  }  
  
  it should "return None for distance to a temperature > flame + ambient temperature" in {
    val t = 300.0
    val flame = Flame(1.0, 0.0, Coord.Origin, 0.5, t)
    
    flame.distanceForTemperature(t + 21.0, 20.0) should be (None)
  }

  it should "calculate distanceForTemperature values which agree with plumeTemperature values" in {
    val depthIgnited = XLim / 10
    val flameLength = depthIgnited + 1.0
    val flameTemp = 250.0
    val ambientTemp = 20.0

    val flame = Flame(flameLength, 0.0, Coord.Origin, depthIgnited, flameTemp)

    // For random distances > depthIgnited, calculate temperature at that distance and then
    // check the inverse calculation of distance for that temperature
    forAll(distances) { d =>
      whenever(d > depthIgnited) {
        val calculatedTemp = flame.plumeTemperature(d, ambientTemp)
        flame.distanceForTemperature(calculatedTemp, ambientTemp).value should be(d +- Tol)
      }
    }
  }
}

