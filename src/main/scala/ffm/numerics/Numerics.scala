package ffm.numerics

import scala.language.implicitConversions 

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
object Numerics {
  
  /**
   * Used to specify the comparison tolerance used by 
   * [Numerics.almostEq]
   */
  case class DoubleTolerance(value: Double) {
    require(value > 0.0, "tolerance value must be > 0.0")
  }

  /**
   * Default comparison tolerance (1.0e-8).
   */
  implicit val DefaultTol = DoubleTolerance(1.0e-8)
  
  def almostEq(a: Double, b: Double)(implicit tolerance: DoubleTolerance) =
    math.abs(a - b) <= tolerance.value

  def gt(a: Double, b: Double) =
    !leq(a, b)
    
  def lt(a: Double, b: Double) =
    !geq(a, b)
    
  def leq(a: Double, b: Double) =
    almostEq(a, b) || a < b
    
  def geq(a: Double, b: Double) =
    almostEq(a, b) || a > b
    
  def almostZero(a: Double) =
    almostEq(a, 0.0)
    
  def clampToZero(a: Double) =
    if (almostZero(a)) 0.0 else a
    
  /**
   * Provides an implicit conversion from Double to Numerics.DoubleEx
   * which adds `almostEq` and `almostZero` methods.
   * 
   * Example:
   * {{{
   * import ffm.numerics.Numerics._
   * 
   * val d1 = 1.0
   * val d2 = d1 + 1e-10
   * 
   * d1 == d2         // false
   * d1.almostEq(d2)  // true
   * }}}
   */
  implicit class DoubleEx(d: Double) {
  
      def almostEq(that: Double) = Numerics.almostEq(d, that)
      def almostZero = Numerics.almostZero(d)
  }  
}