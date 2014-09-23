package ffm.fire

import scala.Vector
import scala.collection.mutable
import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord
import ffm.forest.Species


trait FireModel {
  trait Result
  
  def run(): Result
}

class SingleSiteFireModel(pathModel: IgnitionPathModel, site: Site, includeCanopy: Boolean, fireLineLength: Double) extends FireModel {

  val surfaceWindSpeed = VegetationWindModel.surfaceWindSpeed(site, includeCanopy)
  
  val surfaceFlameLength = site.surface.flameLength(surfaceWindSpeed)
  
  val surfaceFlameAngle = 
    Flame.flameAngle(surfaceFlameLength, surfaceWindSpeed, site.slope, fireLineLength)

  val surfaceFlames: Vector[Flame] = {
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
  def run(): Result = {
    val preHeatingFlames = Vector(
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
      flameConnections.contains(lower.level) || site.isVerticalAssociation(lower, upper)

    for (stratum <- site.strata) {
      val incidentFlames = createIncidentFlames(stratum, isConnected(_, stratum), allSpeciesWeightedFlameSeries)
      val stratumWindSpeed = VegetationWindModel.windSpeedAtHeight(stratum.averageMidHeight, site, includeCanopy)
      
      /*
       * Plant ignitions
       */
      val preHeatingEndTime = -1.0
      val canopyHeatingDistance = 0.0
      for (speciesComp <- stratum.speciesCompositions) {
        val species = speciesComp.species
        
        val runFromPoint = pathModel.generatePath(IgnitionRunType.PlantRun)(
            site, stratum.level, species, 
            incidentFlames, preHeatingFlames, 
            preHeatingEndTime, canopyHeatingDistance, stratumWindSpeed, 
            _: Coord)
        
        val paths = initialCrownIgnitionPoints(species) map (runFromPoint)    
        
      }
    }

    // FIXME !
    return new Result {}
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
    allFlameSeries: Map[StratumLevel, StratumFlameSeries]): Vector[Flame] = {

    if (allFlameSeries.isEmpty) surfaceFlames
    else {
      /*
       * Find lower strata with flames and a connection to the current stratam
       */
      val lowerActiveStrata =
        for {
          otherStratum <- site.strata
          if otherStratum < stratum &&
            allFlameSeries.isDefinedAt(otherStratum.level) &&
            isConnected(otherStratum)
        } yield otherStratum

      /*
       * Calculate a flame-weighted wind speed
       */
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
      val combinedFlames = lowerActiveStrata.foldLeft(surfaceFlames) {
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
  def initialCrownIgnitionPoints(species: Species): Vector[Coord] = {
    val pts = for {
      prop <- -1.0 to 1.0 by 0.5
      x = species.crown.width * prop / 2
      crownPt = species.crown.pointInBase(x)

      // ensure that the point is not below surface
      surfacePt = Coord(x, x * math.tan(site.slope))
      pt = if (crownPt.y > surfacePt.y) crownPt else surfacePt
    } yield pt
    
    pts.toVector
  }

}
