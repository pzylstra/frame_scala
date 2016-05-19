package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.forest.Vegetation
import ffm.forest.VegetationWindModel

/**
 * Holds data for a single run of a [[FireModel]].
 */
case class DefaultFireModelRunResult(
    site: Site,
    canopyEffectIncluded: Boolean,
    surfaceOutcome: SurfaceOutcome,
    pathsAndFlames: Map[StratumLevel, StratumPathsFlames],
    flameSummaries: Map[StratumLevel, StratumFlameSummary],
    combinedFlames: IndexedSeq[Flame],
    ratesOfSpread:  Map[StratumLevel, Double]) extends FireModelRunResult

/**
 * Companion object with a method to create an empty result.
 * 
 * An empty run result has an empty surface outcome and
 * no paths or flames.
 */
object DefaultFireModelRunResult {
  def empty(site: Site, canopyEffectIncluded: Boolean) = DefaultFireModelRunResult(
      site, 
      canopyEffectIncluded = canopyEffectIncluded,
      DefaultSurfaceOutcome.Empty, 
      pathsAndFlames = Map.empty, 
      flameSummaries = Map.empty, 
      combinedFlames = Vector.empty,
      ratesOfSpread = Map.empty)  
}


/**
 * A builder class to progressively add result data before retrieving
 * a final [[FireModelRunResult]] object.
 * 
 * Some ordering is enforced by the `add` methods: surface data must be
 * added first, then stratum outcomes (if any), then combined flames (if any).
 */
class DefaultFireModelRunResultBuilder(site: Site, canopyIncluded: Boolean) {

  private var workingResult = 
    DefaultFireModelRunResult.empty(site, canopyIncluded)


  def addSurfaceOutcome(surf: SurfaceOutcome): Unit = {
    require(workingResult.surfaceOutcome == DefaultSurfaceOutcome.Empty, 
        "Adding a surface outcome when prior data have been set")
    
    workingResult = workingResult.copy(surfaceOutcome = surf)
  }

  def addStratumPathsFlames(pnf: StratumPathsFlames): Unit = {
    require(workingResult.combinedFlames.isEmpty, 
        "Adding a stratum outcome after combined flames")
    
    workingResult = workingResult.copy(pathsAndFlames = workingResult.pathsAndFlames + (pnf.stratum.level -> pnf))
  }

  def addCombinedFlames(flames: IndexedSeq[Flame]): Unit = {
    // If the set of flames is not empty, the existing result object should have
    // one or more stratum outcomes already.
    require(flames.isEmpty || !workingResult.pathsAndFlames.isEmpty,
      "Adding combined flames to a result object with no stratum paths and flames recorded")

    workingResult = workingResult.copy(combinedFlames = flames)
  }

  /**
   * The flame series with the largest flame for each stratum.
   * 
   * TODO: this is here because DefaultSingleSiteFireModel needs it prior to creating the run result,
   *   but it is not a sensible or safe arrangement.
   */
  def largestFlameSeriesPerStratum(): Map[StratumLevel, StratumFlameSeries] =
    for { 
      (level, pnf) <- workingResult.pathsAndFlames
      fs <- StratumPathsFlames.selectMaxFlameSeries(pnf, _.maxFlameLength)
    } yield (level -> fs)

  /**
   * Returns the result object in its current state.
   * 
   * Calling this method causes a flame summary (length, height, angle) and
   * rate of spread to be calculated for each stratum currently recorded.
   *
   * Each call returns a new immutable result object.
   */
  def toResult(windModel: VegetationWindModel, canopyEffectIncluded: Boolean): FireModelRunResult = {
      
    // Calculate flame summaries
    //
    val fsums = (workingResult.pathsAndFlames.map {
      case (level, pnf) =>

        val opFS = StratumPathsFlames.selectMaxFlameSeries(pnf, _.cappedMaxFlameLength)

        val fsum =
          if (opFS.isEmpty) StratumFlameSummary(level, 0.0, 0.0, 0.0)

          else {
            val fs = opFS.get
            val len = fs.cappedMaxFlameLength
            val origin = fs.longestFlame.origin

            val windSpeed = windModel.windSpeedAtHeight(pnf.stratum.averageMidHeight, site, canopyEffectIncluded)

            val angle =
              if (level == StratumLevel.Canopy)
                DefaultFlame.windEffectFlameAngle(len, windSpeed, site.surface.slope)
              else
                DefaultFlame.flameAngle(len, windSpeed, site.surface.slope, site.context.fireLineLength)

            val height = origin.y + len * math.sin(angle) - (origin.x + len * math.cos(angle)) * math.tan(site.surface.slope)

            StratumFlameSummary(level, len, angle, height)
          }

        (level -> fsum)
    }).toMap
    
    workingResult = workingResult.copy(flameSummaries = fsums)

    // Calculate rates of spread for strata other than the canopy
    //
    val ros = DefaultROS.calculateNonCanopy(workingResult)
    
    // Return completed result object
    workingResult.copy(ratesOfSpread = ros)
  }

}

