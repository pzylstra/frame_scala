package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.forest.Species
import ffm.geometry.Coord
import scala.collection.mutable.ArrayBuffer
import ffm.ModelSettings
import ffm.geometry.Segment
import scala.util.control._
import ffm.geometry.Ray
import ffm.geometry.Line
import ffm.numerics.Numerics

/**
 * This is for progressive refactoring of the C++ code for computeIgnitionPath
 *
 * - copy and paste code and initial conversion to compilable Scala
 * - Mikado style progressive refactoring
 */
class SpikeIgnitionPathModel extends IgnitionPathModel {

  import IgnitionRunType._

  def generatePath(
    runType: IgnitionRunType,
    context: IgnitionContext,
    initialPoint: Coord): IgnitionPath = {

    val runner = new Runner(runType, context)
    runner.run(initialPoint)
  }

  /**
   * This inner class is only here to make it easier to structure the path simulation as
   * a series of small functions.
   */
  private class Runner(runType: IgnitionRunType, context: IgnitionContext) {

    // Define some getters so we don't have to write 'context.' all the time
    def site = context.site
    def level = context.stratumLevel
    def species = context.speciesComponent.species 
    def preHeatingFlames = context.preHeatingFlames
    def incidentFlames = context.incidentFlames
    def stratumWindSpeed = context.stratumWindSpeed

    /**
     * Runs the path simulation and returns an IgnitionResult.
     */
    def run(initialPoint: Coord): IgnitionPath = {
      val pathBuilder = IgnitionPathBuilder(runType, context, initialPoint)

      //pop the last pre-heating flame off the vector of pre-heating flames because that level will
      //provide the direct heating
      val activePreHeatingFlames =
        if (preHeatingFlames.isEmpty) preHeatingFlames
        else preHeatingFlames.init

      val plantFlames = ArrayBuffer.empty[Flame]
      val ignitedSegments = ArrayBuffer

      var iPt = initialPoint
      
      //we loop over an indeterminate number of time steps, but no more 
      //than maxTimeSteps counted from when ignition occurs. This is why
      //we use safetyCounter instead of timeStep in the loop condition. 

      val TimeStepLoop = new Breaks
      val PointLoop = new Breaks
      
      TimeStepLoop.breakable {
        for (timeStep <- TimeSteps(from = 1)) {
          // If this is a stratum path run, and we have ignition, adjust the wind speed
          // by the rate of flame progression
          val modifiedWindSpeed = {
            val adjustment =
              if (runType == IgnitionRunType.StratumRun && pathBuilder.hasIgnition) {
                val N = pathBuilder.numSegments
                if (N == 1)
                  math.max(0.0, pathBuilder.head.end.x - initialPoint.x) / ModelSettings.ComputationTimeInterval
                else
                  math.max(0.0, pathBuilder.segments(N - 1).end.x - pathBuilder.segments(N - 2).end.x) / ModelSettings.ComputationTimeInterval

              } else 0.0

            stratumWindSpeed - adjustment
          }

          // plant flame from previous time step (may be None)
          val plantFlame: Option[Flame] = plantFlames.lastOption

          // incident flame for the current time step
          val incidentFlame: Option[Flame] =
            if (timeStep <= incidentFlames.size) Some(incidentFlames(timeStep - 1))
            else None

          if (!(plantFlame orElse incidentFlame).isDefined) {
            TimeStepLoop.break
          }

          val maxPlantPath = potentialPlantPathLength(plantFlame, iPt)
          val maxIncidentPath = potentialIncidentPathLength(incidentFlame, iPt)

          //if there is any more of the plant to burn then compute ignition for the next time step
          var ePt = iPt
          if (maxIncidentPath > 0 || maxPlantPath > 0) {
            //the direction and max possible extent of the next ignition segment is determined by 
            //whichever path has the greatest length
            val (pathLength, pathAngle) =
              if (maxPlantPath > maxIncidentPath) (maxPlantPath, plantFlame.get.angle)
              else (maxIncidentPath, incidentFlame.get.angle)

            val locatedPreHeatingFlames = activePreHeatingFlames map { phf =>
              val origin = locateFlameOrigin(phf.flame, ePt)
              val newFlame = phf.flame.toOrigin(origin)
              phf.copy(flame = newFlame)
            }

            //the possible ignition distance is divided into numPenetrationSteps segments and we test each
            //segment in turn for ignition
            val stepDist = pathLength / ModelSettings.NumPenetrationSteps
            val testPoints = (stepDist to pathLength by stepDist) map (d => iPt.toBearing(pathAngle, d))

            PointLoop.breakable {
              var isFirstTestPoint = true
              for (testPt <- testPoints) {
                var dryingFactor = 1.0
                var dryingTemp = site.temperature

                //compute the drying at testPt from preheating flames. NOTE that we have already popped the last
                //of the preheating flames off the vector because that level will provide the direct heating

                for (phf <- locatedPreHeatingFlames) {
                  val d = testPt.distanceTo(phf.flame.origin)
                  val t = phf.flame.plumeTemperature(d, site.temperature)
                  val duration = phf.duration(context.preHeatingEndTime)
                  dryingFactor *= math.max(0.0, 1.0 - duration / calculateIDT(t))

                  if (timeStep == 1 && iPt.almostEq(initialPoint) && isFirstTestPoint) {
                    pathBuilder.recordPreHeatingFlameDrying(
                      time = timeStep,
                      flame = phf,
                      distanceToFlame = d,
                      dryingFactor = dryingFactor,
                      dryingTemperature = t,
                      duration = duration)
                  }
                }

                //drying from the incident flames, but only if testPt is not already completely dry
                if (dryingFactor > 0) {
                  val N = math.min(timeStep - 1, incidentFlames.size)

                  for (i <- 1 to N) {
                    val flame = incidentFlames(i - 1)
                    val d = locateFlameOrigin(flame, iPt).distanceTo(testPt)
                    val t = flame.plumeTemperature(d, site.temperature)
                    dryingFactor *= math.max(0.0, 1.0 - ModelSettings.ComputationTimeInterval / calculateIDT(t))
                  }
                }

                //drying from the plant flames, but only if testPt is not already completely dry
                if (dryingFactor > 0) {
                  for (flame <- plantFlames) {
                    val d = flame.origin.distanceTo(testPt)
                    val t = flame.plumeTemperature(d, site.temperature)
                    val idt = calculateIDT(t)
                    dryingFactor *= math.max(0.0, 1.0 - ModelSettings.ComputationTimeInterval / idt)
                  }
                }

                val incidentTemp = (for {
                  flame <- incidentFlame
                  origin = locateFlameOrigin(flame, iPt)
                  d = origin.distanceTo(testPt)
                  t = flame.plumeTemperature(d, site.temperature)
                } yield t).getOrElse(0.0)

                val plantTemp = (for {
                  flame <- plantFlame
                  t = flame.plumeTemperature(testPt.distanceTo(flame.origin), site.temperature)
                } yield t).getOrElse(0.0)

                val maxTemp = math.max(incidentTemp, plantTemp)

                val idt = dryingFactor * calculateIDT(maxTemp)

                if (iPt == initialPoint && isFirstTestPoint && incidentFlame.isDefined) {
                  val flame = incidentFlame.get
                  val d = locateFlameOrigin(flame, iPt).distanceTo(testPt)

                  pathBuilder.recordIncidentFlameDrying(
                    time = timeStep,
                    flame = flame,
                    distanceToFlame = d,
                    dryingFactor = dryingFactor,
                    dryingTemperature = maxTemp,
                    ignitionDelayTime = idt)
                }

                //if ignition does not occur for testPt then break from loop over penetration steps
                if (idt > ModelSettings.ComputationTimeInterval || maxTemp < species.ignitionTemperature)
                  PointLoop.break

                //if we get here ignition has occurred, so reset end pt and continue
                ePt = testPt

                isFirstTestPoint = false
              } //end of loop over penetrations steps
            } // PointLoop.breakable
          }

          if (pathBuilder.hasIgnition || ePt.distinctFrom(iPt)) {
            if (!pathBuilder.hasIgnition) {
              pathBuilder.addSegment(timeStep, iPt, ePt)
              plantFlames += newPlantFlame(pathBuilder.last, modifiedWindSpeed)

            } else {
              //compute flame duration and hence start point of new segment
              val flameDuration =
                if (runType == StratumRun &&
                  level == StratumLevel.Canopy &&
                  iPt.x > context.canopyHeatingDistance) {
                  // Flame residence time is reduced for stratum ignition in canopy 
                  // if the canopy has not been heated sufficiently.
                  math.ceil(ModelSettings.ReducedCanopyFlameResidenceTime / ModelSettings.ComputationTimeInterval).toInt
                } else {
                  math.ceil(species.flameDuration / ModelSettings.ComputationTimeInterval).toInt
                }

              val segStart = {
                val n = pathBuilder.numSegments
                if (n < flameDuration) pathBuilder.segments.head.start
                else pathBuilder.segments(n - flameDuration).end
              }

              if (!Numerics.almostZero(maxIncidentPath) || !Numerics.almostZero(maxPlantPath) || segStart.distinctFrom(ePt)) {
                pathBuilder.addSegment(timeStep, segStart, ePt)
                plantFlames += newPlantFlame(pathBuilder.last, modifiedWindSpeed)

              } else {
                TimeStepLoop.break //from loop over time steps
              }
            }
            //reset ignition point
            iPt = ePt
            
            if (timeStep - pathBuilder.ignitionTime >= ModelSettings.MaxIgnitionTimeSteps) TimeStepLoop.break
          }
        } //end of loop over time steps

      } // TimeStepLoop.breakable

      return pathBuilder.toIgnitionPath
    }
    
    /////////////////////////////////////////////////////////////////////////////////////

    /**
     * Calculates the potential ignition distance due to the given plant flame.
     *
     * The flame is passed in as an Option to handle the case when there is no current
     * plant flame.
     */
    def potentialPlantPathLength(plantFlame: Option[Flame], startPoint: Coord): Double =
      (for {
        flame <- plantFlame

        // intersection of the flame's path with the crown (may be None)
        r = Ray(startPoint, flame.angle)

        seg <- species.crown.intersection(r)

        // max distance from the flame with the require ignition temp (may be None)
        ignitLen <- flame.distanceForTemperature(species.ignitionTemperature, site.temperature)

        // resulting path length
        pathLen = math.min(seg.length, ignitLen)

      } yield pathLen).getOrElse(0.0)

    /**
     * Calculates the potential ignition distance due to the given incident flame.
     *
     * The flame is passed in as an Option to handle the case when there is no current
     * incident flame.
     */
    def potentialIncidentPathLength(incidentFlame: Option[Flame], startPoint: Coord): Double =
      (for {
        flame <- incidentFlame
        surfaceLine = Line(Coord(0, 0), site.slope)

        flameOrigin: Coord = runType match {
          case PlantRun =>
            surfaceLine.originOnLine(startPoint, flame.angle).getOrElse(
              throw new Error("Unable to find incident flame origin"))

          case StratumRun => flame.origin
        }

        r = Ray(startPoint, flame.angle)
        seg <- species.crown.intersection(r)

        distForTemp <- flame.distanceForTemperature(species.ignitionTemperature, site.temperature)
        ignitDist = math.max(0.0, distForTemp - startPoint.distanceTo(flameOrigin))

        pathLen = math.min(seg.length, ignitDist)

      } yield pathLen).getOrElse(0.0)

    /**
     * Calculates an ignition delay time at the given temperature.
     */
    def calculateIDT(temperature: Double): Double = {
      val idtProp =
        if (Species.isGrass(species, level)) ModelSettings.GrassIDTReduction
        else 1.0

      species.ignitionDelayTime(temperature) * idtProp
    }

    /*
     * Returns an origin that a flame has if its plume is to pass
     * through a given point.
     */
    def locateFlameOrigin(flame: Flame, targetPoint: Coord): Coord = {
      runType match {
        case PlantRun =>
          val surfaceLine = Line(Coord.Origin, site.slope)

          surfaceLine.originOnLine(targetPoint, flame.angle).getOrElse(
            throw new Error("Unable to find flame origin"))

        case StratumRun => flame.origin
      }
    }

    /*
     * Creates a new plant flame based on the given ignited segment
     */
    def newPlantFlame(segment: IgnitedSegment, windSpeed: Double): Flame = {
      assert(segment.length > 0)

      val flameLen = species.flameLength(segment.length)
      val deltaT =
        if (Species.isGrass(species, level)) ModelSettings.GrassFlameDeltaTemperature
        else ModelSettings.MainFlameDeltaTemperature

      Flame(flameLen,
        Flame.windEffectFlameAngle(flameLen, windSpeed, site.slope),
        segment.start,
        segment.length,
        deltaT)
    }

  }
}
