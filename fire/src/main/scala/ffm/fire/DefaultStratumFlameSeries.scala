package ffm.fire

import ffm.forest.Stratum
import ffm.geometry.Coord

/**
 * Companion object to create StratumFlameSeries instances.
 */
object DefaultStratumFlameSeries {

  import ffm.numerics.Stats
  
  def apply(stratum: Stratum, flames: IndexedSeq[Flame]): StratumFlameSeries = {
    require(!flames.isEmpty, "Cannot create flame series from an empty sequence of flames")

    // Double check that all flames have length > 0
    require(!flames.exists(_.flameLength <= 0.0), "All flame lengths must be greater than zero")

    new DefaultImpl(stratum, flames)
  }

  
  private class DefaultImpl(val stratum: Stratum, _flames: IndexedSeq[Flame]) extends StratumFlameSeries {

    val sortedFlames = _flames.sortBy(_.flameLength).reverse

    val size: Int = sortedFlames.size

    val flameLengths: IndexedSeq[Double] = sortedFlames map (_.flameLength)

    val meanFlameLength: Double = calculateMean(flameLengths).getOrElse(0.0)

    val longestFlame = sortedFlames.head

    val maxFlameLength: Double = longestFlame.flameLength

    val cappedMaxFlameLength: Double =
      math.min(maxFlameLength, meanFlameLength + Stats.stddev(flameLengths, meanFlameLength))

    val meanOrigin: Coord = {
      val x = calculateMean(sortedFlames.map(_.origin.x)).getOrElse(0.0)
      val y = calculateMean(sortedFlames.map(_.origin.y)).getOrElse(0.0)
      Coord(x, y)
    }

    /** Mean depth ignited. */
    val meanDepthIgnited: Double = calculateMean(sortedFlames.map(_.depthIgnited)).getOrElse(0.0)

    /** Mean flame delta temperature. */
    val meanDeltaTemperature: Double = calculateMean(sortedFlames.map(_.deltaTemperature)).getOrElse(0.0)
  }
  
  private def calculateMean(xs: Seq[Double]): Option[Double] = xs match {
    case Seq() => None
    case xs => Some(Stats.mean(xs))
  }

}
