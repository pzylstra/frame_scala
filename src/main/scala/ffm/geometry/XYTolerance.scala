package ffm.geometry

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
  
  /** Uniform tiny X and Y tolerance == 1.0e-8. */
  val Tiny = XYTolerance(1.0e-8)
}