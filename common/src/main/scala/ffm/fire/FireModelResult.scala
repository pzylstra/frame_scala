package ffm.fire

import ffm.forest.StratumLevel
import ffm.geometry.Coord
import ffm.forest.VegetationWindModel
import ffm.forest.Site

/**
 * 
 */
case class FireModelResult(site: Site, fireLineLength: Double, run1: FireModelRunResult, run2: FireModelRunResult) {
  
  val hasSecondRun: Boolean =
    run2.stratumOutcomes.nonEmpty

  case class StratumResult(level: StratumLevel, flameLength: Double, flameAngle: Double, flameHeight: Double)
  
  /*
   * The reference run to use for overall summary statistics.
   */
  private val refRun =
    if (hasSecondRun) run2 else run1

  val stratumResults = refRun.stratumOutcomes.map { outcome =>
    val opFS = outcome.selectFlameSeries { (fs1, fs2) =>
      if (fs1.cappedMaxFlameLength > fs2.cappedMaxFlameLength) fs1 else fs2
    }

    val level = outcome.stratum.level

    if (opFS.isEmpty)
      StratumResult(level, 0.0, 0.0, 0.0)
    else {
      val fs = opFS.get
      val len = fs.cappedMaxFlameLength
      val origin = fs.longestFlame.origin

      val windSpeed = VegetationWindModel.windSpeedAtHeight(outcome.stratum.averageMidHeight, site, !hasSecondRun)

      val angle =
        if (outcome.stratum.level == StratumLevel.Canopy)
          Flame.windEffectFlameAngle(len, windSpeed, site.slope)
        else
          Flame.flameAngle(len, windSpeed, site.slope, fireLineLength)

      val height = origin.y + len * math.sin(angle) - (origin.x + len * math.cos(angle)) * math.tan(site.slope)

      StratumResult(level, len, angle, height)
    }
  }
    
}

