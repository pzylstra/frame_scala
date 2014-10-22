package ffm.fire

import ffm.forest.Stratum
import ffm.geometry.Coord
import ffm.numerics.Stats

/**
 * A series of flames in a given stratum.
 * 
 * The input collection of flames must have at least one flame and all 
 * flames must have length > 0.
 */
class StratumFlameSeries (val stratum: Stratum, _flames: IndexedSeq[Flame]) {
  
  require(!_flames.isEmpty, "Cannot create flame series from an empty sequence of flames")
  
  // Double check that all flames have length > 0
  require( !_flames.exists(_.flameLength <= 0.0), "All flame lengths must be greater than zero" )
  
  /** Flames sorted in descending order of flame length. */
  val flames = _flames.sortBy(_.flameLength).reverse

  /** Number of flames in this flame series. */
  val size: Int = flames.size

  /** Flame lengths (same vector as `flames map (_.flameLength)`) */
  val flameLengths: IndexedSeq[Double] = flames map (_.flameLength)

  /** 
   * Mean flame length. 
   *
   * There are no zero-length flames.
   */
  val meanFlameLength: Double = 
    meanFlameAttr( _.flameLength )
    
  /** Flame with greatest length. */
  val longestFlame = flames.head
  
  /** Maximum flame length. */
  val maxFlameLength: Double = longestFlame.flameLength
  
  /** Maximum flame length capped to mean length plus 1 std dev. */
  val cappedMaxFlameLength: Double = 
    math.max(maxFlameLength, meanFlameLength + Stats.stddev(flameLengths, meanFlameLength))

  /**
   * Mean flame origin.
   *
   * Calculated as the arithmetic mean of origin coordinates.
   */
  val meanOrigin: Coord = {
    val xmean = meanFlameAttr( _.origin.x )
    val ymean = meanFlameAttr( _.origin.y )
    Coord(xmean, ymean)
  }
  
  /** Mean depth ignited. */
  val meanDepthIgnited: Double = meanFlameAttr( _.depthIgnited )
    
  /** Mean flame delta temperature. */
  val meanDeltaTemperature: Double = meanFlameAttr( _.deltaTemperature )
    
  /**
   * Private helper to calculate the mean value of a flame attribute.
   */
  private def meanFlameAttr(f: (Flame) => Double): Double =
    Stats.mean( flames.map(f) )
}

