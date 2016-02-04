package ffm.numerics

import scala.collection.mutable.TreeSet
import scala.collection.mutable.SortedSet

/**
 * A SortedSet implementation for Double values with equality
 * based on the [Numerics.almostEq] method to clamp closely spaced values.
 * 
 * {{{
 * val tolerance = 0.01
 * val set = RoundedDoubleSortedSet(tolerance)
 * set += 1.0
 * set += 0.995
 * set += 1.005
 * set += 2.0
 * println(set)  // prints Set(1.0, 2.0)
 * }}}
 */
final class RoundedDoubleSortedSet(val tolerance: Double) extends SortedSet[Double] {
  
  private val Num = Numerics(tolerance)
  
  /**
   * An ordering for Double values which makes use of [Numerics.almostEq].
   */
  override val ordering: Ordering[Double] = new Ordering[Double] {
    def compare(a: Double, b: Double): Int =
      if (Num.almostEq(a, b)) 0
      else a.compare(b)
  }
  
  private val ts = TreeSet.empty[Double](ordering)

  override def contains(x: Double) = ts.contains(x)
  override def += (x: Double) = { if (!contains(x)) ts += x; this }
  override def -= (x: Double) = { ts -= x; this }  
  override def iterator: Iterator[Double] = ts.iterator
  override def keysIteratorFrom(start: Double): Iterator[Double] = ts.keysIteratorFrom(start)
  override def rangeImpl(from: Option[Double], until: Option[Double]) = ts.rangeImpl(from, until)
}

object RoundedDoubleSortedSet {
  def apply(tolerance: Double): RoundedDoubleSortedSet =
    new RoundedDoubleSortedSet(tolerance)
}
