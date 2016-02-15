package ffm.fire

import ffm.numerics.Stats

/**
 * Utility object for calculating summary statistics for sets of flames.
 */
object FlameCalculator {
  
  /**
   * Calculates the mean of a numeric attribute for the given flames.
   * 
   * Example:
   * {{{
   * // find the mean length of a set of flames
   * val flames = ...
   * val meanLen = FlameCalculator.mean(flames, _.flameLength)
   * }}}
   * 
   * @param flames the flames
   * @param fn a user-provided function to extract the desired attribute from each flame
   */
  def mean(flames: Seq[Flame], fn: (Flame) => Double): Option[Double] = flames match {
    // no flames
    case Seq() => None
    
    // one or more flames
    case _ => Some( Stats.mean( flames.map(fn) ) )
  }

  /**
   * Calculates the maximum of a numeric attribute for the given flames.
   * 
   * Example:
   * {{{
   * // find the maximum length of a set of flames
   * val flames = ...
   * val maxLen = FlameCalculator.max(flames, _.flameLength)
   * }}}
   * 
   * @param flames the flames
   * @param fn a user-provided function to extract the desired attribute from each flame
   */
  def max(flames: Seq[Flame], fn: (Flame) => Double): Option[Double] = flames match {
    // no flames
    case Seq() => None
    
    // one or more flames
    case _ => Some( flames.map(fn).max )
  }
  
}