package ffm.fire

import scala.collection.mutable.ArrayBuffer

import ffm.ModelSettings
import ffm.forest.{Site, Species, Stratum}
import ffm.geometry._
import ffm.util.Options

object IgnitionRunModel {
  
  val PlantRun = 1
  val StratumRun = 2
  
  def plantFlameRun = doRun(PlantRun) _
  
  def stratumFlameRun = doRun(StratumRun) _
  
  private def doRun(runType: Int)(
      site: Site,
      stratumLevel: Stratum.Level,
      species: Species,
      incidentFlames: Vector[Flame],
      preHeatingFlames: Vector[PreHeatingFlame],
      preHeatingEndTime: Double,
      canopyHeatingDistance: Double,
      initialPoint: Coord) 
  {
    require(runType == PlantRun || runType == StratumRun)
    
    // TODO holder for results
    
    // TODO - better to just pass the pre-heating flames under consideration to this method
    // The last pre-heating flame is not considered (it will provide direct heating)
    val actualPreHeatingFlames = if (preHeatingFlames.isEmpty) Vector() else preHeatingFlames.init
        
    val isGrass = Species.isGrass(species, stratumLevel)
    
    val plantFlames = ArrayBuffer.empty[Flame]
    
    // TODO - make this external
    case class IgnitionPath(segments: Vector[Segment]) {
      def hasSegments = !segments.isEmpty
    }
    
    // Helper function to modify wind speed if necessary
    def modifiedWindSpeed(igPath: IgnitionPath): Double = {
      if (runType == StratumRun && igPath.hasSegments) {
        val n = igPath.segments.size
        
        val xstart = 
          if (n == 1) initialPoint.x 
          else igPath.segments(n-2).end.x
          
        val xend = 
          if (n == 1) igPath.segments(0).end.x 
          else igPath.segments(n-1).end.x
          
        site.windSpeed - math.max(0.0, xend - xstart) / ModelSettings.computationTimeInterval
      }
      else
        site.windSpeed  // unmodified
    }
    
    
    // A simple result class to be used by the time step function (below)
    case class TimeStepResult(igPath: IgnitionPath, isFinished: Boolean)
    
    // Helper function to do the calculations for a single time step
    def doTimeStep(timeStep: Int, igPath: IgnitionPath, curPoint: Coord): TimeStepResult = {
      // Plant flame from previous time step
      val plantFlame: Option[Flame] = 
        if (plantFlames.isEmpty) None
        else Some(plantFlames.last)
        
      // Incident flame
      val incidentFlame: Option[Flame] =
        if (timeStep <= incidentFlames.size) Some(incidentFlames(timeStep - 1))
        else None
        
      // Check whether we have any flame - if not finish up
      if (!Options.any(plantFlame, incidentFlame))
        TimeStepResult(igPath, isFinished=true)
        
      else {
        // Maximum potential ignition distance for the plant flame, if present
        val maxPlantPathLen = calculateMaxPlantPathLength(plantFlame, species, curPoint, site)
        
        // Maximum potential ignition distance for the incident flame, if present
        val maxIncidentPathLen = calculateMaxIncidentPathLength(incidentFlame, species, curPoint, site, runType)

        if (maxPlantPathLen > 0 || maxIncidentPathLen > 0) {
          
        }
        
        TimeStepResult(igPath, isFinished=false)
      }
    }
    
    var postIgnitionTimeStep = 0
    var finished = false
    while (!finished && postIgnitionTimeStep < ModelSettings.maxTimeSteps) {
    
    }
  }

  /**
   * Calculates the maximum path length that can be ignited by a plant flame, if present.
   */
  def calculateMaxPlantPathLength(plantFlame: Option[Flame], species: Species, curPoint: Coord, site: Site): Double = {
    (for {
      flame <- plantFlame

      // intersection of the flame's path with the crown (may be None)
      r = Ray(curPoint, flame.angle)
      seg <- species.crown.intersection(r)

      // max distance from the flame with the require ignition temp (may be None)
      ignitLen <- flame.distanceForTemperature(species.ignitionTemp, site.temperature)

      // resulting path length
      pathLen = math.min(seg.length, ignitLen)

    } yield pathLen).getOrElse(0.0)
  }

  /**
   * Calculates the maximum path length that can be ignited by an incident flame, if present.
   */
  def calculateMaxIncidentPathLength(incidentFlame: Option[Flame], species: Species, curPoint: Coord, site: Site, runType: Int): Double = {
    (for {
      flame <- incidentFlame
      surfaceLine = Line(Coord(0, 0), site.slope)

      flameOrigin: Coord = runType match {
        case PlantRun =>
          surfaceLine.originOnLine(curPoint, flame.angle).getOrElse(
            throw new Error("Unable to find incident flame origin"))

        case StratumRun => flame.origin
      }

      r = Ray(curPoint, flame.angle)
      seg <- species.crown.intersection(r)

      distForTemp <- flame.distanceForTemperature(species.ignitionTemp, site.temperature)
      ignitDist = math.max(0.0, distForTemp - curPoint.distanceTo(flameOrigin))

      pathLen = math.min(seg.length, ignitDist)

    } yield pathLen).getOrElse(0.0)

  }
}