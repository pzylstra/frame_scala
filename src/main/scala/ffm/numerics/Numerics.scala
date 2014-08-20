package ffm.numerics

import org.scalautils.TolerantNumerics._
import org.scalautils.TypeCheckedTripleEquals._

object Numerics {
  
  implicit val DblTol = tolerantDoubleEquality(1.0e-9)
  
  def almostEq(a: Double, b: Double) =
    a === b

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
}