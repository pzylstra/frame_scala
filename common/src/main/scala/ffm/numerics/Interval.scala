package ffm.numerics

import scala.math.Numeric

/**
 * A numeric interval.
 */
case class Interval[T](val left: T, val right: T, val leftClosed: Boolean = true, val rightClosed: Boolean = true)(implicit num: Numeric[T]) {
  import num.mkOrderingOps
  
  def contains(x: T): Boolean = 
    leftTest(x) && rightTest(x)
    
  def leftTest(x: T): Boolean =
    if (leftClosed) x >= left
    else x > left
    
  def rightTest(x: T): Boolean =
    if (rightClosed) x <= right
    else x < right
}