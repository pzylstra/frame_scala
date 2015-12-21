package ffm.util

import scala.annotation.tailrec

/**
 * Provides some handy extra methods for IndexedSequences in pimp-my-library style.
 *
 * {{{
 * // import the object and its contents to add extra methods to 
 * // sequences in scope
 * import ffm.util.IndexedSeqUtils._
 * 
 * // Example: the pairs method (sugar for seq.zip( seq.tail ) )
 * val xs = Vector(1, 2, 3, 4)
 * println(xs.pairs)  // prints Vector((1, 2), (2, 3), (3, 4))
 * }}}
 */
object IndexedSeqUtils {

  implicit class IndexedSeqEx[A](seq: IndexedSeq[A]) {
    
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
     * Combines this sequence with another by applying a function to the 
     * overlapping elements and appending any left-over elements.
     * 
     * Let (n, n2) be the lengths of this sequence and seq2.
     * The output sequence consists of min(n, n2) elements, calculated by 
     * applying function f to the overlapping input elements, followed by the
     * the remaining elements of the longer sequence if n != n2.
     */
    def combine(seq2: IndexedSeq[A])(f: (A, A) => A): IndexedSeq[A] = {
      val (n, n2) = (seq.length, seq2.length)
      val common = (seq zip seq2) map { case(a, a2) => f(a, a2) }
      
      if (n == n2) common
      else common ++ (if (n > n2) seq.drop(n2) else seq2.drop(n))
    }

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