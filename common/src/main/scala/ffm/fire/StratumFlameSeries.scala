package ffm.fire

import ffm.forest.Stratum
import ffm.geometry.Coord

/**
 * A series of flames associated with a given stratum.
 */
trait StratumFlameSeries {

  /** Stratum to which these flames apply. */
  def stratum: Stratum

  /** Flames sorted in descending order of length. */
  def sortedFlames: IndexedSeq[Flame]

  /** Number of flames. */
  def size: Int

  /** Flame lengths (m) in same order as sortedFlames. */
  def flameLengths: IndexedSeq[Double]

  /** The longest flame. */
  def longestFlame: Flame

  /** Mean flame length (m). */
  def meanFlameLength: Double

  /** Maximum flame length (m). */
  def maxFlameLength: Double

  /**
   * Capped maximum flame length (m).
   *
   * This is the minimum of maxFlameLength and (meanFlameLength + sd) where
   * sd is the standard deviation of flame lengths.
   */
  def cappedMaxFlameLength: Double

  /** Mean flame origin (arithmetic mean of origin coordinates). */
  def meanOrigin: Coord

  /** Mean depth ignited (m). */
  def meanDepthIgnited: Double

  /** Mean flame temperature (degrees C). */
  def meanDeltaTemperature: Double

}

