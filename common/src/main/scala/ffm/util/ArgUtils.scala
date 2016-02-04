package ffm.util

/**
 * Provides methods to check argument values for common `require` cases.
 */
object ArgUtils {

  def isNotNegative(name: String, x: Double) {
    require(x >= 0.0, s"$name cannot have a negative value (got $x)")
  }

  def isPositive(name: String, x: Double) {
    require(x > 0.0, s"$name must have a positive value (got $x)")
  }

  def isProportion(name: String, x: Double, allowZero: Boolean) {
    if (allowZero)
      require(x >= 0.0 && x <= 1.0, s"$name must be a proportion (got $x)")
    else
      require(x > 0.0 && x <= 1.0, s"$name must be a proportion greater than zero (got $x)")
  }

}