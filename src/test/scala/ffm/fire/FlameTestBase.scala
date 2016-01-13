package ffm.fire

import org.scalacheck.Gen
import org.scalatest.OptionValues
import org.scalatest.prop.PropertyChecks

import ffm.BasicSpec
import ffm.geometry.Coord

/**
 * Provides ScalaCheck generators for the Flame units tests.
 */
abstract class FlameTestBase extends BasicSpec with PropertyChecks with OptionValues {

  /*
   * Generators for ordinate values, Coords, angles etc.
   */
  val XLim = 10.0
  val YLim = 10.0
  val SlopeLim = math.Pi / 6
  val WindLim = 50.0
  
  val validXY: Gen[(Double, Double)] =
    for {
      x <- Gen.choose(-XLim, XLim)
      y <- Gen.choose(-YLim, YLim)
    } yield (x, y)
  
  val coords: Gen[Coord] = for ((x, y) <- validXY) yield Coord(x, y)
  
  val angles: Gen[Double] = Gen.choose(-math.Pi, math.Pi)
  
  val distances: Gen[Double] = Gen.choose(0.0, XLim)
  
  val slopes: Gen[Double] = Gen.choose(-SlopeLim, SlopeLim)
  
  val windSpeeds: Gen[Double] = Gen.choose(-WindLim, WindLim)
  
  /*
   * General tolerance for +- comparisons
   */
  val Tol = ffm.numerics.Numerics.DefaultTolerance

  
}