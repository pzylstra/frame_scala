package ffm.fire

import scala.Vector
import scala.collection.mutable

import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord

object VegetationFireModel {
  
  /**
   * FIXME - return a result !
   */
  def ignitionRun(site: Site, includeCanopy: Boolean, fireLineLength: Double) {
    val surfaceWindSpeed = VegetationWindModel.surfaceWindSpeed(site, includeCanopy)
    
    val surfaceFlameLength = site.surface.flameLength(surfaceWindSpeed)
    
    val surfaceFlameAngle = Flame.flameAngle(surfaceFlameLength, surfaceWindSpeed, site.slope, fireLineLength)

    val numFlames = math.round(site.surface.flameResidenceTime / ComputationTimeInterval).toInt

    // A function to make copies of the surface flame
    val makeFlame = () => Flame(
        length = surfaceFlameLength, 
        angle = surfaceFlameAngle, 
        origin = Coord.Origin, 
        depthIgnited = 0, 
        deltaTemperature = MainFlameDeltaTemperature)
    
    val surfaceFlames = Vector.fill(numFlames)(makeFlame())

    val preHeatingFlames = Vector(
        PreHeatingFlame(makeFlame(), StratumLevel.Surface, startTime=0, endTime=site.surface.flameResidenceTime)
    )
    
    val allSpeciesWeightedFlameSeries = scala.collection.mutable.Map.empty[StratumLevel, StratumFlameSeries]
    
    /*
     * If a level exists in flameConnections then a flame connection with 
     * higher strata is guaranteed from this level, else have to look at the
     * strata overlaps.
     */  
    val flameConnections = mutable.Set.empty[StratumLevel]
    
    def hasConnection(lower: StratumLevel, upper: StratumLevel) =
      flameConnections.contains(lower) || site.isVerticalAssociation(lower, upper)
    
    for (stratum <- site.strata) {
      /*
       * Compute incident flames for this stratum from surface flames and the
       * flames from lower strata. Only include lower strata that have a connection
       * with this stratum.
       */
      val incidentFlames =
        if (allSpeciesWeightedFlameSeries.isEmpty) surfaceFlames
        else {
          /*
           * Find lower strata with flames and a connection to the current stratam
           */
          val lowerActiveStrata =
            for {
              otherStratum <- site.strata
              if otherStratum < stratum &&
                allSpeciesWeightedFlameSeries.isDefinedAt(otherStratum.level) &&
                hasConnection(otherStratum.level, stratum.level)
            } yield otherStratum

          /*
           * Calculate a flame-weighted wind speed
           */
          val initLen = surfaceFlames.head.flameLength
          val initWind = surfaceWindSpeed * surfaceFlames.head.flameLength

          val (finalLen, finalWind) =
            lowerActiveStrata.foldLeft((initLen, initWind)) {
              case ((curLen, curWind), lower) =>
                val fs = allSpeciesWeightedFlameSeries(lower.level)

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
              val lowerFlames = allSpeciesWeightedFlameSeries(lower.level).flames

              Flame.combineFlames(flames, lowerFlames, flameWeightedWindSpeed, site.slope, fireLineLength)
          }

          // Return the combined flames as the incident flames
          combinedFlames
        }
    }
  }
}
