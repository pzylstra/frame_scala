package ffm.fire

import ffm.geometry.Coord
import ffm.forest.Stratum

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

    val meanFlameLength: Double = FlameCalculator.mean(sortedFlames, _.flameLength).getOrElse(0.0)

    val longestFlame = sortedFlames.head

    val maxFlameLength: Double = longestFlame.flameLength

    val cappedMaxFlameLength: Double =
      math.min(maxFlameLength, meanFlameLength + Stats.stddev(flameLengths, meanFlameLength))

    val meanOrigin: Coord = {
      val x = FlameCalculator.mean(sortedFlames, _.origin.x).getOrElse(0.0)
      val y = FlameCalculator.mean(sortedFlames, _.origin.y).getOrElse(0.0)
      Coord(x, y)
    }

    /** Mean depth ignited. */
    val meanDepthIgnited: Double = FlameCalculator.mean(sortedFlames, _.depthIgnited).getOrElse(0.0)

    /** Mean flame delta temperature. */
    val meanDeltaTemperature: Double = FlameCalculator.mean(sortedFlames, _.deltaTemperature).getOrElse(0.0)
  }

}
