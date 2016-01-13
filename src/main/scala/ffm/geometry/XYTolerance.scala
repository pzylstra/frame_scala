package ffm.geometry

import ffm.numerics.Numerics

/**
 * Stores tolerance values in the X and Y direction.
 * 
 * Used by [[ffm.geometry.Coord]] for comparisons.
 * 
 * The input values must be >= 0.0 
 */
case class XYTolerance(xtol: Double, ytol: Double) {
  require(xtol >= 0.0 && ytol >= 0.0, "tolerances must >= 0")
}

/**
 * Companion object to the XYTolerance class.
 */
object XYTolerance {
  
  /** Creates an XYTolerance with equal X and Y tolerance values. */
  def apply(tol: Double): XYTolerance =
    XYTolerance(tol, tol)
  
  /** Default tolerance with value [Numerics.DistanceTolerance] for both 
   *  X and Y directions. 
   */
  val Default = XYTolerance(Numerics.DistanceTolerance)
}