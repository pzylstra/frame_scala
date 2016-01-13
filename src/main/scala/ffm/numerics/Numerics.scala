package ffm.numerics

import scala.language.implicitConversions 

import ffm.ModelSettings

/**
 * Provides methods for comparison of Double values within a specified 
 * tolerance.
 * 
 * The default tolerance is suitable for the range of expected values in 
 * the model (e.g. +-1000).
 * 
 * Examples:
 * {{{
 * import ffm.numerics.Numerics._
 * 
 * val CoarseTol = DoubleTolerance(0.1)
 * val FineTol = DoubleTolerance(1.0e-8)
 * 
 * val a = 1.0
 * val b = 1.01
 * 
 * almostEq(a, b)(CoarseTol)  // returns true
 * almostEq(a, b)(FineTol)    // returns false
 * 
 * // With default tolerance
 * almostEq(a, b)  // returns false
 * 
 * // Implicit value to set tolerance for all method calls within 
 * // the same scope
 * implicit val MyTol = DoubleTolerance(0.1)
 * 
 * almostEq(a, b)    // returns true
 * almostZero(0.05)  // true
 * }}}
 */
trait Numerics {
  def almostEq(a: Double, b: Double): Boolean
  def gt(a: Double, b: Double): Boolean
  def lt(a: Double, b: Double): Boolean
  def leq(a: Double, b: Double): Boolean
  def geq(a: Double, b: Double): Boolean
  def almostZero(a: Double): Boolean
  def clampToZero(a: Double): Double
}

object Numerics {
  
  val DefaultTolerance = 1.0e-8
  val DistanceTolerance = math.pow(10, -ModelSettings.DistancePrecision)
  
  val Default: Numerics = new NumericsImpl(DefaultTolerance)
  val Distance: Numerics = new NumericsImpl(DistanceTolerance)
  
  def apply(tolerance: Double = DefaultTolerance): Numerics =
    new NumericsImpl(tolerance)

  private class NumericsImpl(tolerance: Double) extends Numerics {

    def almostEq(a: Double, b: Double): Boolean =
      math.abs(a - b) <= tolerance

    def gt(a: Double, b: Double): Boolean =
      !leq(a, b)

    def lt(a: Double, b: Double): Boolean =
      !geq(a, b)

    def leq(a: Double, b: Double): Boolean =
      almostEq(a, b) || a < b

    def geq(a: Double, b: Double): Boolean =
      almostEq(a, b) || a > b

    def almostZero(a: Double): Boolean =
      almostEq(a, 0.0)

    def clampToZero(a: Double): Double =
      if (almostZero(a)) 0.0 else a
  }
      
}
