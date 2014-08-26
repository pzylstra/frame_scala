package ffm.geometry

case class XYTolerance(xtol: Double, ytol: Double) {
  require(xtol >= 0.0 && ytol >= 0.0, "tolerances must >= 0")
}

object XYTolerance {
  
  /** Creates an instance with equal X and Y tolerances. */
  def apply(tol: Double): XYTolerance =
    XYTolerance(tol, tol)
  
  /** Default instance with X and Y tolerance == 1.0e-8. */
  val default = XYTolerance(1.0e-8)
}