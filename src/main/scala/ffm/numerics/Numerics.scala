package ffm.numerics

import scala.language.implicitConversions 

/**
 * Provides methods for comparison of Double values within a specified 
 * tolerance.
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
 */
object Numerics {
  
  /**
   * Used to specify the comparison tolerance used by 
   * [Numerics.almostEq]
   */
  case class DoubleTolerance(tol: Double)

  /**
   * Default comparison tolerance (1.0e-9).
   */
  implicit val Tol = DoubleTolerance(1.0e-9)
  
  def almostEq(a: Double, b: Double)(implicit tolerance: DoubleTolerance) =
    math.abs(a - b) <= tolerance.tol

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
}