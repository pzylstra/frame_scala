package ffm.numerics

import org.apache.commons.math3.stat.StatUtils

/**
 * Provides univariate statistics methods.
 * 
 * A wrapper for functions provided by the Apache Commons Math library.
 */
object Stats {

  /**
   * Sample standard deviation.
   */
  def stddev(xs: Seq[Double]): Double =
    math.sqrt( StatUtils.variance(xs.toArray) )
  
  /**
   * Sample standard deviation with a pre-calculated mean.
   */
  def stddev(xs: Seq[Double], xmean: Double) =
    math.sqrt( StatUtils.variance(xs.toArray, xmean) )
}