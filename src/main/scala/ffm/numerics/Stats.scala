package ffm.numerics

import org.apache.commons.math3.stat.StatUtils

/**
 * Provides univariate statistics methods.
 * 
 * A wrapper for functions provided by the [[http://commons.apache.org/proper/commons-math/ Apache Commons Math]]
 * library.
 */
object Stats {
  
  def mean(xs: Seq[Double]): Double =
    StatUtils.mean(valid(xs).toArray)

  /**
   * Sample standard deviation.
   */
  def stddev(xs: Seq[Double]): Double =
    math.sqrt( StatUtils.variance(valid(xs).toArray) )
  
  /**
   * Sample standard deviation with a pre-calculated mean.
   */
  def stddev(xs: Seq[Double], xmean: Double) =
    math.sqrt( StatUtils.variance(valid(xs).toArray, xmean) )
   
  /**
   * Returns the input sequence if non-empty, or throws an 
   * IllegalArgumentExcpetion otherwise.
   */ 
  private def valid(xs: Seq[Double]) = {
    require(!xs.isEmpty, "Input value sequence is empty")
    xs
  }
}
