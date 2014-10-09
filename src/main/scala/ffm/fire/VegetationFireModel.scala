package ffm.fire

import scala.collection.mutable

import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.Species
import ffm.forest.SpeciesComponent
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord
import ffm.geometry.CrownPoly
import ffm.geometry.Ray

trait FireModel {
  def run(): FireModelResult
}

case class FireModelResult(paths: IndexedSeq[IgnitionPath])

class SingleSiteFireModel(pathModel: IgnitionPathModel, plantFlameModel: PlantFlameModel)(site: Site, includeCanopy: Boolean, fireLineLength: Double) extends FireModel {

  val surfaceWindSpeed = VegetationWindModel.surfaceWindSpeed(site, includeCanopy)

  val surfaceFlameLength = site.surface.flameLength(surfaceWindSpeed)

  val surfaceFlameAngle =
    Flame.flameAngle(surfaceFlameLength, surfaceWindSpeed, site.slope, fireLineLength)

  val surfaceFlames: IndexedSeq[Flame] = {
    val nflames = math.round(site.surface.flameResidenceTime / ComputationTimeInterval).toInt
    Vector.fill(nflames) {
      Flame(
        length = surfaceFlameLength,
        angle = surfaceFlameAngle,
        origin = Coord.Origin,
        depthIgnited = 0,
        deltaTemperature = MainFlameDeltaTemperature)
    }
  }

  /**
   * Runs the fire model.
   */
  def run(): FireModelResult = {
    val ignitionPathBuf = mutable.ArrayBuffer.empty[IgnitionPath]

    val preHeatingFlames = IndexedSeq(
      PreHeatingFlame(surfaceFlames.head, StratumLevel.Surface, startTime = 0, endTime = site.surface.flameResidenceTime))

    // FIXME - use recursion instead of a var here ?
    var allSpeciesWeightedFlameSeries = Map.empty[StratumLevel, StratumFlameSeries]

    /*
     * If a level exists in flameConnections then a flame connection with 
     * higher strata is guaranteed from this level, else have to look at the
     * strata overlaps.
     */
    val flameConnections = mutable.Set.empty[StratumLevel]

    def isConnected(lower: Stratum, upper: Stratum) =
      flameConnections.contains(lower.level) || site.vegetation.isVerticalAssociation(lower, upper)

    for (stratum <- site.vegetation.strata) {
      val incidentFlames = createIncidentFlames(stratum, isConnected(_, stratum), allSpeciesWeightedFlameSeries)
      val stratumWindSpeed = VegetationWindModel.windSpeedAtHeight(stratum.averageMidHeight, site, includeCanopy)

      // FIXME - Get rid of these vars by making this block recursive
      var preHeatingEndTime = -1.0
      var canopyHeatingDistance = 0.0
      var cumulativePreHeatingStartTime = 0.0

      val plantRunContext = IgnitionContext(
        IgnitionRunType.PlantRun,
        site,
        stratum.level,
        preHeatingFlames,
        incidentFlames,
        preHeatingEndTime,
        canopyHeatingDistance,
        stratumWindSpeed)

      val plantPathFn = pathModel.generatePath(plantRunContext, plantFlameModel) _

      val plantRunResult = findPlantIgnitionPaths(stratum, plantPathFn)
      ignitionPathBuf ++= plantRunResult.paths

      if (plantRunResult.isIgnition) {
        val mergedFlameLengths = plantRunResult.flameAttr.flameLengths map { flen =>
          Flame.lateralMergedFlameLength(flen, fireLineLength, stratum.averageWidth, stratum.modelPlantSep)
        }

        val plantFlames = (0 until plantRunResult.flameAttr.size) map { i =>
          val length = mergedFlameLengths(i)
          val angle = Flame.windEffectFlameAngle(length, stratumWindSpeed, site.slope)
          Flame(length, angle, plantRunResult.flameAttr.origins(i), plantRunResult.flameAttr.flameDepths(i), plantRunResult.flameAttr.temperatures(i))
        }

        cumulativePreHeatingStartTime += plantRunResult.flameAttr.timeToLongestFlame
        preHeatingEndTime = cumulativePreHeatingStartTime

        if (stratum.level == StratumLevel.Canopy) {

        /*
         * TODO - Canopy heating logic
         */
        }

        val stratumRunContext = plantRunContext.copy(
          runType = IgnitionRunType.StratumRun,
          preHeatingFlames = Vector.empty,
          incidentFlames = plantFlames,
          preHeatingEndTime = 0.0,
          canopyHeatingDistance = canopyHeatingDistance)

        val stratumPathFn = pathModel.generatePath(stratumRunContext, plantFlameModel) _

        val stratumRunResult = findStratumIgnitionPaths(
          stratum, plantFlames.head.origin, plantFlames.head.angle, stratumPathFn)

        ignitionPathBuf ++= stratumRunResult.paths
      }
    }

    FireModelResult(ignitionPathBuf.toVector)
  }

  /**
   * Holds results from a plant or stratum ignition run.
   */
  private case class IgnitionRunResult(paths: IndexedSeq[IgnitionPath], isIgnition: Boolean, flameAttr: WeightedFlameAttributes)

  /**
   * Runs the ignition path simulation for all species in the given stratum.
   */
  private def findPlantIgnitionPaths(stratum: Stratum, pathFn: (SpeciesComponent, Coord) => IgnitionPath): IgnitionRunResult = {
    val bestPaths = for {
      spComp <- stratum.speciesComponents

      paths = initialCrownIgnitionPoints(spComp.species) map { pos =>
        pathFn(spComp, pos)
      }
    } yield (paths reduce selectBestPath)

    val isIgnition = bestPaths exists (_.hasIgnition)
    
    val flameAttr = WeightedFlameAttributes(plantFlameModel)(bestPaths)

    IgnitionRunResult(bestPaths, isIgnition, flameAttr)
  }

  /**
   * Runs the ignition paths simulation for all species in the given stratum
   * using an artificial crown based on average stratum canopy attributes.
   */
  private def findStratumIgnitionPaths(
    stratum: Stratum,
    flameOrigin: Coord,
    flameAngle: Double,
    pathFn: (SpeciesComponent, Coord) => IgnitionPath): IgnitionRunResult = {

    val stratumCrown = createStratumCrown(stratum)
    val ray = Ray(flameOrigin, flameAngle)
    val crossing = stratumCrown.intersection(ray)

    if (crossing.isEmpty) IgnitionRunResult(Vector.empty, false, WeightedFlameAttributes.Empty)
    else {
      val initialPt = crossing.get.start

      val paths = for {
        spComp <- stratum.speciesComponents
        proxySpecies = createProxyStratumSpecies(spComp.species, stratumCrown, stratum)
        proxyComponent = SpeciesComponent(proxySpecies, spComp.weighting)
        path = pathFn(proxyComponent, crossing.get.start)

      } yield path

      val isIgnition = paths exists (_.hasIgnition)

      val flameAttr = WeightedFlameAttributes(plantFlameModel)(paths)
      
      IgnitionRunResult(paths, isIgnition, flameAttr)
    }
  }

  /**
   * Compares two IgnitionPaths and selects the 'best'.
   *
   * If only one path has ignition it is selected. If both have ignition, the path
   * with the longest ignited segment is selected. If neither have ignition,
   * the path with the highest drying temperature is selected.
   */
  private def selectBestPath(a: IgnitionPath, b: IgnitionPath): IgnitionPath = {
    if (a.hasIgnition)
      if (b.hasIgnition) if (b.maxSegmentLength > a.maxSegmentLength) b else a
      else a
    else if (b.hasIgnition) b
    else if (b.maxDryingTemperature > a.maxDryingTemperature) b else a
  }

  /**
   * Tests if ignition paths for a given species / stratum give rise to flames extending
   * beyond the species crown.
   *
   * TODO: this calculation is based on ignited segment but possibly duplicates code
   * in the main run function where we are creating plant flames. Check with Phil
   * about this.
   */
  private def isFlameBeyondCrown(species: Species, path: IgnitionPath, windSpeed: Double, slope: Double): Boolean = {
    path.segmentsByLengthAndTime exists { seg =>
      val flameLen = plantFlameModel.flameLength(species, seg.length)
      val flameTipX = seg.start.x + flameLen * math.cos(Flame.windEffectFlameAngle(flameLen, windSpeed, slope))
      flameTipX > species.crown.width / 2
    }
  }

  /**
   * Creates a vector of incident flames for the given stratum.
   *
   * Compute incident flames for this stratum from surface flames and the
   * flames from lower strata. Only include lower strata that have a connection
   * with this stratum.
   */
  private def createIncidentFlames(
    stratum: Stratum,
    isConnected: (Stratum) => Boolean,
    allFlameSeries: Map[StratumLevel, StratumFlameSeries]): IndexedSeq[Flame] = {

    if (allFlameSeries.isEmpty) surfaceFlames
    else {
      // Find lower strata with flames and a connection to the 
      // current stratum
      val lowerActiveStrata =
        for {
          otherStratum <- site.vegetation.strata
          if otherStratum < stratum &&
            allFlameSeries.isDefinedAt(otherStratum.level) &&
            isConnected(otherStratum)
        } yield otherStratum

      // Calculate a flame-weighted wind speed
      val initLen = surfaceFlames.head.flameLength
      val initWind = surfaceWindSpeed * surfaceFlames.head.flameLength

      val (finalLen, finalWind) =
        lowerActiveStrata.foldLeft((initLen, initWind)) {
          case ((curLen, curWind), lower) =>
            val fs = allFlameSeries(lower.level)

            (curLen + fs.cappedMaxFlameLength,
              curWind + fs.cappedMaxFlameLength * VegetationWindModel.windSpeedAtHeight(lower.averageMidHeight, site, includeCanopy))
        }

      val flameWeightedWindSpeed = if (finalLen > 0) finalWind / finalLen else 0.0

      /*
       * Combine surface flames with those from lower active strata to 
       * create the incident flames
       */
      val combinedFlames: IndexedSeq[Flame] = lowerActiveStrata.foldLeft(surfaceFlames) {
        case (flames, lower) =>
          val lowerFlames = allFlameSeries(lower.level).flames

          Flame.combineFlames(flames, lowerFlames, flameWeightedWindSpeed, site.slope, fireLineLength)
      }

      // Return the combined flames as the incident flames
      combinedFlames
    }
  }

  /**
   * Returns the coordinates of candidate ignition points across the base of
   * a species crown.
   */
  def initialCrownIgnitionPoints(species: Species): IndexedSeq[Coord] = {
    val pts = for {
      prop <- -1.0 to 1.0 by 0.5
      x = species.crown.width * prop / 2
      crownPt = species.crown.pointInBase(x)

      // ensure that the point is not below surface
      surfacePt = Coord(x, x * math.tan(site.slope))
      pt = if (crownPt.y > surfacePt.y) crownPt else surfacePt
    } yield pt

    pts
  }

  /**
   * Creates an artificial crown for a stratum flame run.
   */
  def createStratumCrown(stratum: Stratum): CrownPoly = {
    val minx = stratum.modelPlantSep - stratum.averageWidth / 2
    val maxx = minx + StratumBigCrownWidth

    val tanSlope = math.tan(site.slope)

    val vertices = Vector(
      Coord(minx, stratum.averageBottom + minx * tanSlope),
      Coord(minx, stratum.averageTop + minx * tanSlope),
      Coord(maxx, stratum.averageTop + maxx * tanSlope),
      Coord(maxx, stratum.averageBottom + maxx * tanSlope))

    CrownPoly(vertices)
  }

  /**
   * Creates a proxy for a given species to use in a stratum flame run.
   */
  def createProxyStratumSpecies(sp: Species, crown: CrownPoly, stratum: Stratum): Species = {
    Species(name = sp.name,
      crown = crown,
      liveLeafMoisture = sp.liveLeafMoisture,
      deadLeafMoisture = sp.deadLeafMoisture,
      propDead = sp.propDead,
      ignitionTemp = Some(sp.ignitionTemperature),
      leafForm = sp.leafForm,
      leafThickness = sp.leafThickness,
      leafWidth = sp.leafWidth,
      leafLength = sp.leafLength,
      leafSeparation = sp.leafSeparation,
      stemOrder = sp.stemOrder,
      clumpDiameter = sp.clumpDiameter,
      clumpSeparation = math.max(sp.clumpSeparation, stratum.modelPlantSep - stratum.averageWidth))
  }

}
