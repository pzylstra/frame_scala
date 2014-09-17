package ffm.util

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
      
  }
  
}