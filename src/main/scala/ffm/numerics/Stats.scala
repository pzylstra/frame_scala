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
    doMean(nonempty(xs))

  /**
   * Sample standard deviation.
   */
  def stddev(xs: Seq[Double]): Double =
    doStddev(nonempty(xs))
  
  /**
   * Sample standard deviation with a pre-calculated mean.
   */
  def stddev(xs: Seq[Double], xmean: Double) =
    math.sqrt( StatUtils.variance(nonempty(xs).toArray, xmean) )
   
  /**
   * Returns the input sequence if non-empty, or throws an 
   * IllegalArgumentExcpetion otherwise.
   */ 
  private def nonempty(xs: Seq[Double]) = {
    require(!xs.isEmpty, "Input value sequence is empty")
    xs
  }

  /**
   * Calculates mean without prior checking of input sequence.
   */
  private def doMean(xs: Seq[Double]): Double = 
    StatUtils.mean(xs.toArray)

  /**
   * Calculates sample standard deviation without prior checking
   * of the input sequence.
   */
  private def doStddev(xs: Seq[Double]): Double =
    math.sqrt( StatUtils.variance(xs.toArray) )

}
