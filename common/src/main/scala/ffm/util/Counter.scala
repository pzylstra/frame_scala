package ffm.util

import scala.math.Numeric

object Counter {
  def apply[T](from: T, step: T)(implicit num: Numeric[T]) = new Iterator[T] {
    private var cur = from
    
    def hasNext = true
    
    def next(): T = {
      val rtn = cur
      cur = num.plus(cur, step)
      rtn
    }
  }
}

object IntCounter {
  def apply(from: Int = 0, step: Int = 1) = Counter[Int](from, step)
}