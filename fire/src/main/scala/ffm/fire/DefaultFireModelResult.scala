package ffm.fire

import ffm.geometry.Coord
import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.forest.VegetationWindModel

/**
 * Aggregates results from separate runs of a fire model, with and without a canopy layer.
 */
class DefaultFireModelResult(
    site: Site, 
    fireLineLength: Double, 
    windModel: VegetationWindModel,
    run1: FireModelRunResult, 
    run2: FireModelRunResult) extends FireModelResult {
  
  private val hasSecondRun: Boolean =
    run2.stratumOutcomes.nonEmpty

  /*
   * The reference run to use for overall summary statistics.
   */
  private val refRun =
    if (hasSecondRun) run2 else run1

  val stratumResults: IndexedSeq[StratumFlameSummary] = refRun.stratumOutcomes.map { outcome =>
    
    val opFS = outcome.selectFlameSeries { (fs1, fs2) =>
      if (fs1.cappedMaxFlameLength > fs2.cappedMaxFlameLength) fs1 else fs2
    }

    val level = outcome.stratum.level

    if (opFS.isEmpty)
      StratumFlameSummary(level, 0.0, 0.0, 0.0)
      
    else {
      val fs = opFS.get
      val len = fs.cappedMaxFlameLength
      val origin = fs.longestFlame.origin

      val windSpeed = windModel.windSpeedAtHeight(outcome.stratum.averageMidHeight, site, !hasSecondRun)

      val angle =
        if (outcome.stratum.level == StratumLevel.Canopy)
          DefaultFlame.windEffectFlameAngle(len, windSpeed, site.surface.slope)
        else
          DefaultFlame.flameAngle(len, windSpeed, site.surface.slope, fireLineLength)

      val height = origin.y + len * math.sin(angle) - (origin.x + len * math.cos(angle)) * math.tan(site.surface.slope)

      StratumFlameSummary(level, len, angle, height)
    }
  }
  
  val runResults = Vector(run1, run2)
    
}

