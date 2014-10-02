package ffm.fire

/**
 * Generates a sequence of consecutive integer time step values.
 * 
 * {{{
 * import scala.util.control._
 * Breaks.breakable {
 *   for (timeStep <- TimeSteps(from=1)) {
 *     // do things
 *     ...
 *   
 *     // when finished
 *     break
 *   }
 * }
 * }}}
 */
object TimeSteps {
  def apply(from: Int) = new Iterator[Int] {
    private var i = from
    
    def hasNext = true
    
    def next() = {
      val nextVal = i 
      i += 1
      nextVal
    }
  }
  
}