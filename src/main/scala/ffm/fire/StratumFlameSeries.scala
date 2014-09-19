package ffm.fire

import ffm.forest.StratumLevel
import ffm.numerics.Stats

/**
 * A time-series of flames in a given stratum
 */
class StratumFlameSeries private(val level: StratumLevel, val flames: Vector[Flame]) {
  
  // Double check that all flames have length > 0
  require( !flames.exists(_.flameLength <= 0.0), "All flame lengths must be greater than zero" )

  /**
   * Creates a copy of this flame series with a new flame appended.
   * 
   * {{{
   * val updatedSeries = priorSeries :+ newFlame
   * }}}
   */
  def :+ (flame: Flame): StratumFlameSeries =
    new StratumFlameSeries(level, flames :+ flame)
  
  /** Number of flames in this flame series. */
  val numFlames: Int = flames.size

  /** Flame lengths (same vector as `flames map (_.flameLength)`) */
  val flameLengths: Vector[Double] = flames map (_.flameLength)

  /** 
   * Mean flame length. 
   *
   * There are no zero-length flames.
   */
  val meanFlameLength: Double = flameLengths.sum / numFlames

  /**
   * Maximum flame length.
   */
  val maxFlameLength: Double = flameLengths.max
  
  /**
   * Maximum flame length capped to mean length plus 1 std dev.
   */
  val cappedMaxFlameLength: Double = 
    math.max(maxFlameLength, meanFlameLength + Stats.stddev(flameLengths, meanFlameLength))
  
}


object StratumFlameSeries {
  
  /**
   * Creates an initial series with no flames.
   */
  def apply(level: StratumLevel): StratumFlameSeries =
    new StratumFlameSeries(level, Vector.empty)
  
}