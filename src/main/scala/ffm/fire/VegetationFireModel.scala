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
import ffm.geometry.Line

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
    val flameConnections = new FlameConnections

    val preHeatingFlames = mutable.ArrayBuffer(
      PreHeatingFlame(surfaceFlames.head, StratumLevel.Surface, startTime = 0, endTime = site.surface.flameResidenceTime))

    val allSpeciesWeightedFlameSeries = mutable.ArrayBuffer.empty[StratumFlameSeries]

    // FIXME - Get rid of these vars by making this block recursive
    var preHeatingEndTime = -1.0
    var cumulativePreHeatingStartTime = 0.0

    for (stratum <- site.vegetation.strata) {
      val incidentFlames = createIncidentFlames(stratum, flameConnections, allSpeciesWeightedFlameSeries)
      val stratumWindSpeed = VegetationWindModel.windSpeedAtHeight(stratum.averageMidHeight, site, includeCanopy)

      val plantRunContext = IgnitionContext(
        IgnitionRunType.PlantRun,
        site,
        stratum.level,
        preHeatingFlames,
        incidentFlames,
        preHeatingEndTime,
        canopyHeatingDistance = 0.0,
        stratumWindSpeed
        )

      val plantRunResult = findPlantIgnitionPaths(stratum, plantRunContext)
      ignitionPathBuf ++= plantRunResult.paths

      if (plantRunResult.isIgnition) {
        if (plantRunResult.pathsWithIgnition exists (path => isFlameBeyondCrown(path, stratumWindSpeed)))
          flameConnections.add(stratum)

        val plantFlames = getPlantFlames(plantRunResult, stratumWindSpeed)

        cumulativePreHeatingStartTime += plantRunResult.flameAttr.timeToLongestFlame
        preHeatingEndTime = cumulativePreHeatingStartTime

        val canopyHeatingDistance =
          if (stratum.level == StratumLevel.Canopy) calculateCanopyHeatingDistance(stratum, allSpeciesWeightedFlameSeries)
          else 0.0
          
        val stratumRunContext = plantRunContext.copy(
          runType = IgnitionRunType.StratumRun,
          preHeatingFlames = Vector.empty,
          incidentFlames = plantFlames,
          preHeatingEndTime = 0.0,
          canopyHeatingDistance = canopyHeatingDistance)

        val stratumRunResult = findStratumIgnitionPaths(stratum, stratumRunContext, plantFlames.head)
        ignitionPathBuf ++= stratumRunResult.paths

        val stratumFlames = getStratumFlames(stratumRunResult, stratumWindSpeed)

        val bigFlames =
          if (maxFlameLength(plantFlames) > maxFlameLength(stratumFlames)) plantFlames
          else stratumFlames

        val flameSeries = new StratumFlameSeries(stratum, bigFlames)

        allSpeciesWeightedFlameSeries += flameSeries

        preHeatingFlames += createPreHeatingFlame(
            flameSeries, startTime = cumulativePreHeatingStartTime, windSpeed = stratumWindSpeed)
            
        /*
         * TODO: logic for connection based on comparing max plant flame length to max stratum flame length.
         * 
         * In runs of the C++ model this never seems to be used. 
         * 
         * Ask Phil about this.
         */
      }
    }

    FireModelResult(ignitionPathBuf.toVector)
  }
  

  /**
   * Holds results from a plant or stratum ignition run.
   */
  private case class IgnitionRunResult(stratum: Stratum, paths: IndexedSeq[IgnitionPath], isIgnition: Boolean, flameAttr: WeightedFlameAttributes) {
    
    def pathsWithIgnition = paths filter (_.hasIgnition)
  
  }
  
  /**
   * Records flame connections between strata.
   */
  private class FlameConnections {
    private val connectedLevels = mutable.Set.empty[StratumLevel]
    
    def add(s: Stratum) {
      connectedLevels += s.level
    }

    def isConnected(lower: Stratum, upper: Stratum) =
      connectedLevels.contains(lower.level) || site.vegetation.isVerticalAssociation(lower, upper)
  }

  /**
   * Returns flames generated from a plant ignition run.
   * 
   * If there was no ignition an empty collection is returned.
   */
  private def getPlantFlames(plantRunResult: IgnitionRunResult, stratumWindSpeed: Double): IndexedSeq[Flame] = {
    if (!plantRunResult.isIgnition) Vector.empty
    else {
      val avWidth = plantRunResult.stratum.averageWidth
      val plantSep = plantRunResult.stratum.modelPlantSep
      
      val mergedFlameLengths = plantRunResult.flameAttr.flameLengths map { flen =>
        Flame.lateralMergedFlameLength(flen, fireLineLength, avWidth, plantSep)
      }

      val flames = (0 until plantRunResult.flameAttr.size) map { i =>
        val length = mergedFlameLengths(i)
        val angle = Flame.windEffectFlameAngle(length, stratumWindSpeed, site.slope)
        Flame(length, angle, plantRunResult.flameAttr.origins(i), plantRunResult.flameAttr.flameDepths(i), plantRunResult.flameAttr.temperatures(i))
      }

      flames
    }
  }

  /** 
   * Returns flames generated from a stratum ignition run.
   * 
   * If there was no ignition an empty collection is returned.
   */
  private def getStratumFlames(stratumRunResult: IgnitionRunResult, stratumWindSpeed: Double): IndexedSeq[Flame] = {
    if (!stratumRunResult.isIgnition) Vector.empty
    else {
      val attr = stratumRunResult.flameAttr
      val flames = (0 until attr.size) map { i =>
        val angle = Flame.windEffectFlameAngle(attr.flameLengths(i), stratumWindSpeed, site.slope)
        Flame(attr.flameLengths(i), angle, attr.origins(i), attr.flameDepths(i), attr.temperatures(i))
      }
      
      flames
    }
  }
  
  /**
   * Runs the ignition path simulation for all species in the given stratum.
   */
  private def findPlantIgnitionPaths(stratum: Stratum, context: IgnitionContext): IgnitionRunResult = {
    // Create function to generate ignition paths
    val pathFn = pathModel.generatePath(context, plantFlameModel) _
    
    // Simulate paths for each species in the stratum, selecting the 'best'
    // path for each
    val bestPaths = stratum.speciesComponents map { spComp =>
      val initPts = initialCrownIgnitionPoints(spComp.species)
      val paths = initPts map ( p => pathFn(spComp, p) )
      paths reduce selectBestPath
    }

    val isIgnition = bestPaths exists (_.hasIgnition)
    val flameAttr = WeightedFlameAttributes(plantFlameModel)(bestPaths)

    IgnitionRunResult(stratum, bestPaths, isIgnition, flameAttr)
  }

  /**
   * Runs the ignition paths simulation for all species in the given stratum
   * using an artificial crown based on average stratum canopy attributes.
   */
  private def findStratumIgnitionPaths(stratum: Stratum, context: IgnitionContext, referenceFlame: Flame): IgnitionRunResult = {
    val stratumCrown = createStratumCrown(stratum)
    val ray = Ray(referenceFlame.origin, referenceFlame.angle)
    val crossing = stratumCrown.intersection(ray)

    if (crossing.isEmpty) IgnitionRunResult(stratum, Vector.empty, false, WeightedFlameAttributes.Empty)
    else {
      val pathFn = pathModel.generatePath(context, plantFlameModel) _
      val initialPt = crossing.get.start

      val paths = for {
        spComp <- stratum.speciesComponents
        proxySpecies = createProxyStratumSpecies(spComp.species, stratumCrown, stratum)
        proxyComponent = SpeciesComponent(proxySpecies, spComp.weighting)
        path = pathFn(proxyComponent, crossing.get.start)
      } yield path

      val isIgnition = paths exists (_.hasIgnition)

      val flameAttr = WeightedFlameAttributes(plantFlameModel)(paths)
      
      IgnitionRunResult(stratum, paths, isIgnition, flameAttr)
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
    if (a.hasIgnition) {
      if (b.hasIgnition) if (b.maxSegmentLength > a.maxSegmentLength) b else a
      else a
    }
    else if (b.hasIgnition) b
    else if (b.maxDryingTemperature > a.maxDryingTemperature) b else a
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
   * Tests if any ignited segments in an IgnitionPath give rise to flames extending
   * beyond the species crown.
   *
   * TODO: this calculation is based on ignited segment but possibly duplicates code
   * in the main run function where we are creating plant flames. Check with Phil
   * about this.
   */
  private def isFlameBeyondCrown(path: IgnitionPath, windSpeed: Double): Boolean = {
    path.segmentsByLengthAndTime exists { seg =>
      val species = path.speciesComponent.species
      val flameLen = plantFlameModel.flameLength(species, seg.length)
      val flameTipX = seg.start.x + flameLen * math.cos(Flame.windEffectFlameAngle(flameLen, windSpeed, site.slope))
      flameTipX > species.crown.width / 2
    }
  }

  /**
   * Finds the maximum flame length in a collection of flames.
   */
  private def maxFlameLength(flames: IndexedSeq[Flame]): Double =
    flames.foldLeft(0.0)((len, flame) => len max flame.flameLength)

  /**
   * Creates a vector of incident flames for the given stratum.
   *
   * Compute incident flames for this stratum from surface flames and the
   * flames from lower strata. Only include lower strata that have a connection
   * with this stratum.
   */
  private def createIncidentFlames(
    stratum: Stratum,
    flameConnections: FlameConnections,
    allFlameSeries: IndexedSeq[StratumFlameSeries]): IndexedSeq[Flame] = {

    val flameSeriesByLevel = Map() ++ (allFlameSeries map (fs => (fs.stratum.level, fs)))

    if (allFlameSeries.isEmpty) surfaceFlames
    else {
      // Find lower strata with flames and a connection to the 
      // current stratum
      val lowerActiveStrata =
        for {
          otherStratum <- site.vegetation.strata
          if otherStratum < stratum &&
            flameSeriesByLevel.isDefinedAt(otherStratum.level) &&
            flameConnections.isConnected(stratum, otherStratum)
        } yield otherStratum

      // Calculate a flame-weighted wind speed
      val initLen = surfaceFlames.head.flameLength
      val initWind = surfaceWindSpeed * surfaceFlames.head.flameLength

      val (finalLen, finalWind) =
        lowerActiveStrata.foldLeft((initLen, initWind)) {
          case ((curLen, curWind), lower) =>
            val fs = flameSeriesByLevel(lower.level)

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
          val lowerFlames = flameSeriesByLevel(lower.level).flames

          Flame.combineFlames(flames, lowerFlames, flameWeightedWindSpeed, site.slope, fireLineLength)
      }

      // Return the combined flames as the incident flames
      combinedFlames
    }
  }

  /**
   * Creates a PreHeatingFlame based on the given flame series.
   */
  private def createPreHeatingFlame(flameSeries: StratumFlameSeries, startTime: Double, windSpeed: Double) = {
    val mfl = flameSeries.meanFlameLength
    val angle = Flame.windEffectFlameAngle(mfl, windSpeed, site.slope)
    val flame = Flame(mfl, angle, flameSeries.meanOrigin, flameSeries.meanDepthIgnited, flameSeries.meanDeltaTemperature)
    val endTime = startTime + flameSeries.size * ComputationTimeInterval

    PreHeatingFlame(flame, flameSeries.stratum.level, startTime, endTime)
  }



  /**
   * Calculates canopy heating distance given the canopy stratum and the collection
   * of flame series for lower strata.
   */
  def calculateCanopyHeatingDistance(canopyStratum: Stratum, allFlameSeries: IndexedSeq[StratumFlameSeries]): Double = {

    require(canopyStratum.level == StratumLevel.Canopy)

    // Check that we haven't somehow got a flame series for the canopy already
    require(!allFlameSeries.exists(_.stratum.level == StratumLevel.Canopy), "Flame series already created for canopy stratum")

    val canopyLine = Line(Coord(0.0, canopyStratum.averageBottom), site.slope)

    /*
     * Recursive helper to process the flame series sequence
     */
    def iter(fss: IndexedSeq[StratumFlameSeries], offsetX: Double, curDist: Double): Double = {
      if (fss.isEmpty) curDist - offsetX
      else {
        val flame = fss.head.longestFlame
        val intersectionPt = canopyLine.intersection(flame.plume).get

        val nextDist = {
          val d = flame.origin.distanceTo(intersectionPt)
          if (flame.plumeTemperature(d, site.temperature) >= MinTempForCanopyHeating)
            curDist max (intersectionPt.x + offsetX)
          else
            curDist
        }

        val nextOffsetX =
          if (fss.tail.isEmpty) offsetX
          else {
            val nextfs = fss.tail.head
            val nextStratumLine = Line(Coord(0.0, nextfs.stratum.averageBottom), site.slope)
            val pt = nextStratumLine.intersection(flame.plume).get

            offsetX + pt.x
          }

        iter(fss.tail, nextOffsetX, nextDist)
      }
    }

    iter(allFlameSeries, 0.0, 0.0)
  }

}
