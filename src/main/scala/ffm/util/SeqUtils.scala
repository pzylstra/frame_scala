package ffm.util

import scala.annotation.tailrec

/**
 * Provides some handy extra methods for sequences in a pimp-my-library fashion.
 *
 * {{{
 * // import the object and its contents to add extra methods to 
 * // sequences in scope
 * import ffm.util.SeqUtils._
 * 
 * // Example: the pairs method (sugar for seq.zip( seq.tail ) )
 * val xs = Vector(1, 2, 3, 4)
 * println(xs.pairs)  // prints Vector((1, 2), (2, 3), (3, 4))
 * }}}
 */
object SeqUtils {

  implicit class SeqEx[A](seq: Seq[A]) {
    
    /**
     * Returns the sequence of 2-tuples consisting of successive pairs
     * of values in the input sequence.
     * 
     * This is just syntactic sugar for seq.iterator.zip( seq.tail.iterator )
     */
    def pairs: Iterator[(A, A)] = 
      if (seq.isEmpty) Iterator.empty
      else seq.iterator zip(seq.tail.iterator)

    /**
     * Performs a fold left operation on the sequence, performing a test on 
     * the interim results as it goes.
     * 
     * Example:
     * {{{
     * import ffm.util.SeqUtils._
     * 
     * // sum squares of the input values while cumulative total is < 20
     * val xs = (1 to 10).toList
     * val op = (sum: Int, x: Int) => { println(x); sum + x*x }
     * val result = xs.foldLeftWhile(0)(op)(sum < 20) // prints 1
     *                                                //        2
     *                                                //        3
     *                                                //        4
     * 
     * }}}
     */
    def foldLeftWhile[B](start: B)(op: (B, A) => B)(cond: (B) => Boolean): B = {
      @tailrec
      def iter(remaining: Seq[A], prev: B): B = {
        if (remaining.isEmpty) prev
        else {
          val next = op(prev, remaining.head)
          if (cond(next)) iter(remaining.tail, next)
          else prev
        }
      }

      iter(seq, start)
    }
  }
  
}