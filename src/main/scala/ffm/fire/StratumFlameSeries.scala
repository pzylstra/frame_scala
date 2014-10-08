package ffm.fire

import ffm.forest.StratumLevel
import ffm.numerics.Stats

/**
 * A series of flames in a given stratum
 */
case class StratumFlameSeries (level: StratumLevel, flames: IndexedSeq[Flame]) {
  
  // Double check that all flames have length > 0
  require( !flames.exists(_.flameLength <= 0.0), "All flame lengths must be greater than zero" )

  /** Number of flames in this flame series. */
  val numFlames: Int = flames.size

  /** Flame lengths (same vector as `flames map (_.flameLength)`) */
  val flameLengths: IndexedSeq[Double] = 
    if (flames.isEmpty) Vector()
    else flames map (_.flameLength)

  /** 
   * Mean flame length. 
   *
   * There are no zero-length flames.
   */
  val meanFlameLength: Double = 
    if (flames.isEmpty) 0.0
    else flameLengths.sum / numFlames

  /**
   * Maximum flame length.
   */
  val maxFlameLength: Double = 
    if (flames.isEmpty) 0.0
    else flames.map(_.flameLength).max
  
  /**
   * Maximum flame length capped to mean length plus 1 std dev.
   */
  val cappedMaxFlameLength: Double = 
    if (flames.isEmpty) 0.0
    else math.max(maxFlameLength, meanFlameLength + Stats.stddev(flameLengths, meanFlameLength))
  
}

