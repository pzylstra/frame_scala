package ffm.fire

import scala.collection.mutable.ArrayBuffer
import scala.util.control._
import ffm.ModelSettings._
import ffm.forest.DefaultSpeciesUtils
import ffm.forest.Species
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.geometry._
import ffm.numerics.Numerics
import ffm.util.IntCounter

class DefaultIgnitionPathModel extends IgnitionPathModel {

import IgnitionRunType._

  override def generatePath(context: IgnitionContext, plantFlameModel: PlantFlameModel)
    (speciesComponent: SpeciesComponent, initialPoint: Coord): IgnitionPath = {

    val runner = new Runner(context, plantFlameModel, speciesComponent)
    runner.run(initialPoint)
  }

  /**
   * This inner class is only here to make it easier to structure the path simulation as
   * a series of smaller functions.
   */
  private class Runner(context: IgnitionContext, plantFlameModel: PlantFlameModel, speciesComponent: SpeciesComponent) {

    // Some getters to make the code easier to read
    def runType = context.runType
    def site = context.site
    def level = context.stratumLevel
    def species = speciesComponent.species
    def preHeatingFlames = context.preHeatingFlames
    def incidentFlames = context.incidentFlames
    def stratumWindSpeed = context.stratumWindSpeed
    
    /**
     * Runs the path simulation and returns an IgnitionResult.
     */
    def run(initialPoint: Coord): IgnitionPath = {
      val pathBuilder = IgnitionPathBuilder(context, speciesComponent, initialPoint)

      //pop the last pre-heating flame off the vector of pre-heating flames because that level will
      //provide the direct heating
      val activePreHeatingFlames =
        if (preHeatingFlames.isEmpty) preHeatingFlames
        else preHeatingFlames.init

      val plantFlames = ArrayBuffer.empty[Flame]

      var iPt = initialPoint
      
      //we loop over an indeterminate number of time steps, but no more 
      //than maxTimeSteps counted from when ignition occurs. This is why
      //we use safetyCounter instead of timeStep in the loop condition. 

      val TimeStepLoop = new Breaks
      val PointLoop = new Breaks
      
      TimeStepLoop.breakable {
        for (timeStep <- IntCounter(from = 1, step = 1)) {
          // If this is a stratum path run, and we have ignition, adjust the wind speed
          // by the rate of flame progression
          val modifiedWindSpeed = {
            val adjustment =
              if (runType == IgnitionRunType.StratumRun && pathBuilder.hasIgnition) {
                val N = pathBuilder.numSegments
                if (N == 1)
                  math.max(0.0, pathBuilder.head.end.x - initialPoint.x) / ComputationTimeInterval
                else
                  math.max(0.0, pathBuilder.segments(N - 1).end.x - pathBuilder.segments(N - 2).end.x) / ComputationTimeInterval

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
          if (Numerics.Distance.gt(maxIncidentPath, 0.0) || Numerics.Distance.gt(maxPlantPath, 0.0)) {
            //the direction and max possible extent of the next ignition segment is determined by 
            //whichever path has the greatest length
            val (pathLength, pathAngle) =
              if (maxPlantPath > maxIncidentPath) (maxPlantPath, plantFlame.get.angle)
              else (maxIncidentPath, incidentFlame.get.angle)

            val locatedPreHeatingFlames = activePreHeatingFlames map { phf =>
              val origin = locateFlameOrigin(phf.flame, ePt)
              DefaultPreHeatingFlame.toOrigin(phf, origin)
            }

            //the possible ignition distance is divided into numPenetrationSteps segments and we test each
            //segment in turn for ignition
            val stepDist = pathLength / NumPenetrationSteps
            val testPoints = (1 to NumPenetrationSteps) map (i => iPt.toBearing(pathAngle, i * stepDist))

            PointLoop.breakable {
              var isFirstTestPoint = true
              
              var stepNum = 0;  // debug
              
              for (testPt <- testPoints) {
                var dryingFactor = 1.0
                var dryingTemp = site.weather.temperature
                
                stepNum += 1  // debug

                //compute the drying at testPt from preheating flames. NOTE that we have already popped the last
                //of the preheating flames off the vector because that level will provide the direct heating

                for (phf <- locatedPreHeatingFlames) {
                  val d = testPt.distanceTo(phf.flame.origin)
                  val t = phf.flame.plumeTemperature(d, site.weather.temperature)
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
                    val t = flame.plumeTemperature(d, site.weather.temperature)
                    dryingFactor *= math.max(0.0, 1.0 - ComputationTimeInterval / calculateIDT(t))
                  }
                }

                //drying from the plant flames, but only if testPt is not already completely dry
                if (dryingFactor > 0) {
                  for (flame <- plantFlames) {
                    val d = flame.origin.distanceTo(testPt)
                    val t = flame.plumeTemperature(d, site.weather.temperature)
                    val idt = calculateIDT(t)
                    dryingFactor *= math.max(0.0, 1.0 - ComputationTimeInterval / idt)
                  }
                }

                val incidentTemp = (for {
                  flame <- incidentFlame
                  origin = locateFlameOrigin(flame, iPt)
                  d = origin.distanceTo(testPt)
                  t = flame.plumeTemperature(d, site.weather.temperature)
                } yield t).getOrElse(0.0)

                val plantTemp = (for {
                  flame <- plantFlame
                  t = flame.plumeTemperature(testPt.distanceTo(flame.origin), site.weather.temperature)
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
                if (idt > ComputationTimeInterval || maxTemp < species.ignitionTemperature)
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
                  math.ceil(ReducedCanopyFlameResidenceTime / ComputationTimeInterval).toInt
                } else {
                  math.ceil(plantFlameModel.flameDuration(species) / ComputationTimeInterval).toInt
                }

              val segStart = {
                val n = pathBuilder.numSegments
                if (n < flameDuration) pathBuilder.segments.head.start
                else pathBuilder.segments(n - flameDuration).end
              }

              if (!Numerics.Distance.almostZero(maxIncidentPath) || 
                  !Numerics.Distance.almostZero(maxPlantPath) || 
                  segStart.distinctFrom(ePt)) {
                pathBuilder.addSegment(timeStep, segStart, ePt)
                plantFlames += newPlantFlame(pathBuilder.last, modifiedWindSpeed)

              } else {
                TimeStepLoop.break //from loop over time steps
              }
            }
            //reset ignition point
            iPt = ePt
            
            if (timeStep - pathBuilder.ignitionTimeStep >= MaxIgnitionTimeSteps) TimeStepLoop.break
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
        ignitLen <- flame.distanceForTemperature(species.ignitionTemperature, site.weather.temperature)

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
        surfaceLine = Line(Coord(0, 0), site.surface.slope)

        flameOrigin: Coord = runType match {
          case PlantRun =>
            surfaceLine.originOnLine(startPoint, flame.angle).getOrElse(
              throw new Error("Unable to find incident flame origin"))

          case StratumRun => flame.origin
        }

        r = Ray(startPoint, flame.angle)
        seg <- species.crown.intersection(r)

        distForTemp <- flame.distanceForTemperature(species.ignitionTemperature, site.weather.temperature)
        ignitDist = math.max(0.0, distForTemp - startPoint.distanceTo(flameOrigin))

        pathLen = math.min(seg.length, ignitDist)

      } yield pathLen).getOrElse(0.0)

    /**
     * Calculates an ignition delay time at the given temperature.
     */
    def calculateIDT(temperature: Double): Double = {
      val idtProp =
        if (DefaultSpeciesUtils.isGrass(species, level)) GrassIDTReduction
        else 1.0

      plantFlameModel.ignitionDelayTime(species, temperature) * idtProp
    }

    /*
     * Returns an origin that a flame has if its plume is to pass
     * through a given point.
     */
    def locateFlameOrigin(flame: Flame, targetPoint: Coord): Coord = {
      runType match {
        case PlantRun =>
          val surfaceLine = Line(flame.origin, site.surface.slope)

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

      val flameLen = plantFlameModel.flameLength(species, segment.length)
      val deltaT =
        if (DefaultSpeciesUtils.isGrass(species, level)) GrassFlameDeltaTemperature
        else MainFlameDeltaTemperature

      DefaultFlame(flameLen,
        DefaultFlame.windEffectFlameAngle(flameLen, windSpeed, site.surface.slope),
        segment.start,
        segment.length,
        deltaT)
    }

  }
}
