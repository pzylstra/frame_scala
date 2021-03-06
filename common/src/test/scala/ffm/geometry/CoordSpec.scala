package ffm.geometry

import org.scalatest.prop.PropertyChecks

import org.scalacheck.Gen
import org.scalacheck.Arbitrary._

import ffm.BasicSpec
import ffm.numerics.Numerics

/*
 * Tests for the Coord class. These use property-based (ScalaCheck) testing to
 * exercise the class methods with random values.
 */
class CoordSpec extends BasicSpec with PropertyChecks {

  val Tol = 1.0e-8
  
  val origin = Coord(0, 0)
  
  /*
   * Generators for ordinate values, Coords, angles etc.
   */
  val XLim = 1.0e6
  val YLim = 1.0e6
  
  val validXY: Gen[(Double, Double)] =
    for {
      x <- Gen.choose(-XLim, XLim)
      y <- Gen.choose(-YLim, YLim)
    } yield (x, y)
  
  val coords: Gen[Coord] = for ((x, y) <- validXY) yield Coord(x, y)
  
  val angles: Gen[Double] = Gen.choose(-math.Pi, math.Pi)
  
  val distances: Gen[Double] = Gen.choose(0.0, XLim)
  
    
  "A Coord" should "report the correct ordinates" in {
    forAll (validXY) { (xy) =>
      val (x, y) = xy
      val c = Coord(x, y)
      c.x should be (x +- Tol)
      c.y should be (y +- Tol)
    }
  }
  
  it should "report the correct distance to another Coord" in {
    forAll (coords, coords) { (c0, c1) =>
      val (dx, dy) = (c1.x - c0.x, c1.y - c0.y)
      val obsDistance = math.sqrt(dx * dx + dy * dy)
      
      c0.distanceTo(c1) should be (obsDistance +- Tol)
      c1.distanceTo(c0) should be (obsDistance +- Tol)
    }
  }
  
  it should "report zero distance to itself" in {
    forAll (coords) { (c) => 
      c.distanceTo(c) should be (0.0)
    }
  }
  
  it should "always be almostEq to itself" in {
    forAll (coords) { (c) => 
      c.almostEq(c) should be (true)
    }
  }
  
  it should "correctly report almostEq" in {
    forAll (coords, angles, Gen.choose(0.0, 2 * Numerics.DistanceTolerance)) { (c, angle, d) => 
      val c2 = c.toBearing(angle, d)
      val expected = Numerics.Distance.almostZero(d)
      c.almostEq(c2) should be (expected)
    }
  }
  
  it should "never be distinctFrom itself" in {
    forAll (coords) { (c) => 
      c.distinctFrom(c) should be (false)
    }
  }
  
  it should  "correctly report distinctFrom" in {
    forAll (coords, angles, Gen.choose(0.0, 2 * Numerics.DistanceTolerance)) { (c, angle, d) => 
      val c2 = c.toBearing(angle, d)
      val expected = Numerics.Distance.gt(d, 0.0)
      c.distinctFrom(c2) should be (expected)
    }
  }
  
  it should "create the correct Coord with the toOffset method" in {
    forAll (coords, validXY) { (c0, xy) =>
      val (dx, dy) = xy
      val c1 = c0.toOffset(dx, dy)
      c1.x should be ((c0.x + dx) +- Tol)
      c1.y should be ((c0.y + dy) +- Tol)
    }
  }

  it should "create the correct Coord with the toBearing method" in {
    forAll (validXY, angles, distances) { (xy, angle, distance) =>
      val (x, y) = xy
      val c0 = Coord(x, y)
      val c1 = c0.toBearing(angle, distance)
      
      // Compare angles and distances proportionally to account for 
      // acceptable round-off errors
      val expectedAngle = math.atan2(c1.y - c0.y, c1.x - c0.x)
      expectedAngle / angle should be(1.0 +- Tol)

      val (dx, dy) = (c1.x - c0.x, c1.y - c0.y)
      val expectedDistance = math.sqrt(dx * dx + dy * dy)
      expectedDistance / distance should be (1.0 +- Tol)
    }
  }
  
  it should "report the correct angle to another Coord" in {
    forAll (coords, coords) { (c0, c1) =>
      val expectedAngle = math.atan2(c1.y - c0.y, c1.x - c0.x)
      c0.angleTo(c1) should be (expectedAngle +- Tol)
    }
  }
  
  it should "report a zero angle to itself" in {
    forAll (coords) { (c) => 
      c.angleTo(c) should be (0.0)
    }
  }
  
  it should "report the correct reverse angle" in {
    forAll(coords, coords) { (c0, c1) =>
      val reverseAngle = Angles.normalizePi(c0.angleTo(c1) + math.Pi)
      c1.angleTo(c0) should be (reverseAngle +- Tol)
    }
  }
  
  it should "multiplied by a scalar should return the correct Coord result" in {
    // random coords multipled by values drawn from [-100, 100], making sure to include 0.0
    forAll(coords, Gen.chooseNum(-100.0, 100.0, 0.0)) { (c, m) =>
      val cprod = c.multipliedBy(m)
      cprod.x should be (c.x * m +- Tol)
      cprod.y should be (c.y * m +- Tol)
    }
  }

}