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
    initialPoint: Coord): IgnitionResult = {

    println("*"*20)
    println(s"in generatePath with initial point $initialPoint")
    
    val resultBuilder = IgnitionResultBuilder(runType, context)
    
    // Define some getters so we don't have to write 'context.' all the time
    def site = context.site
    def level = context.stratumLevel
    def species = context.species
    def preHeatingFlames = context.preHeatingFlames
    def incidentFlames = context.incidentFlames
    def stratumWindSpeed = context.stratumWindSpeed 
    
    
    ///////////////////////////////////////////////////////////////////////////
    //
    // Helper functions
    //
    ///////////////////////////////////////////////////////////////////////////
    
    /*
     * Calculates an ignition delay time due to the given flame.
     */
    def calculateFlameIDT(flame: Flame, flameOriginForCalc: Coord, testPoint: Coord): Double = {
      val d = testPoint.distanceTo(flameOriginForCalc)
      val t = flame.plumeTemperature(d, site.temperature)
      calculateIDT(t)
    }


    /*
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
    def newPlantFlame(modifiedWindSpeed: Double): Flame = {
      val seg = resultBuilder.segments.last
      val len = seg.start.distanceTo(seg.end)
      assert(len > 0)

      val flameLen = species.flameLength(len)
      val deltaT =
        if (Species.isGrass(species, level)) ModelSettings.GrassFlameDeltaTemperature
        else ModelSettings.MainFlameDeltaTemperature

      Flame(flameLen,
        Flame.windEffectFlameAngle(flameLen, modifiedWindSpeed, site.slope),
        seg.start,
        len,
        deltaT)
    }

    //pop the last pre-heating flame off the vector of pre-heating flames because that level will
    //provide the direct heating
    val activePreHeatingFlames =
      if (preHeatingFlames.isEmpty) preHeatingFlames
      else preHeatingFlames.init

    val plantFlames = ArrayBuffer.empty[Flame]
    val ignitedSegments = ArrayBuffer

    var iPt = initialPoint
    var ignition = false
    var safetyCounter = 1

    //we loop over an indeterminate number of time steps, but no more 
    //than maxTimeSteps counted from when ignition occurs. This is why
    //we use safetyCounter instead of timeStep in the loop condition. 

    var timeStep = 0

    val TimeStepLoop = new Breaks
    val PointLoop = new Breaks

    TimeStepLoop.breakable {
      while (safetyCounter <= ModelSettings.MaxIgnitionTimeSteps) {
        timeStep += 1

        //for plant flame, and only if required, we modify wind speed by 
        //reducing it by speed of flame progression
        val modifiedWindSpeed = {
          if (runType == IgnitionRunType.StratumRun && resultBuilder.hasIgnition) {
            val sz = resultBuilder.segments.size
            if (sz == 1)
              stratumWindSpeed -
                math.max(0.0, resultBuilder.segments(0).end.x - initialPoint.x) / ModelSettings.ComputationTimeInterval
            else
              stratumWindSpeed -
                math.max(0.0, resultBuilder.segments(sz - 1).end.x - resultBuilder.segments(sz - 2).end.x) / ModelSettings.ComputationTimeInterval
          } else stratumWindSpeed
        }

        //get plant flame from previous time step
        val plantFlame = plantFlames.lastOption

        //get incident flame
        val incidentFlame =
          if (timeStep <= incidentFlames.size) Some(incidentFlames(timeStep - 1))
          else None

        // Original comment ---
        //if no plant flame or incident flame decide what to do
        //we allow for the possibility that incident flame may flare up later
        //though this is not possible under current model because of reordering
        // ---
        // 
        // Here we just break out of the time step loop
        if (!(plantFlame.isDefined || incidentFlame.isDefined)) {
          TimeStepLoop.break
        }

        //compute potential ignition distance for plant flame 
        val maxPlantPath: Double =
          (for {
            flame <- plantFlame

            // intersection of the flame's path with the crown (may be None)
            r = Ray(iPt, flame.angle)
            seg <- species.crown.intersection(r)

            // max distance from the flame with the require ignition temp (may be None)
            ignitLen <- flame.distanceForTemperature(species.ignitionTemperature, site.temperature)

            // resulting path length
            pathLen = math.min(seg.length, ignitLen)

          } yield pathLen).getOrElse(0.0)

        //compute incident flame characteristics. maxIncidentPath is the length of the segment in the 
        //plant that could potentially be ignited by the incident flame, based on iPt, geometry of the
        //plant crown, plume temperature and plant ignition temperature
        val maxIncidentPath = (for {
          flame <- incidentFlame
          surfaceLine = Line(Coord(0, 0), site.slope)

          flameOrigin: Coord = runType match {
            case PlantRun =>
              surfaceLine.originOnLine(iPt, flame.angle).getOrElse(
                throw new Error("Unable to find incident flame origin"))

            case StratumRun => flame.origin
          }

          r = Ray(iPt, flame.angle)
          seg <- species.crown.intersection(r)

          distForTemp <- flame.distanceForTemperature(species.ignitionTemperature, site.temperature)
          ignitDist = math.max(0.0, distForTemp - iPt.distanceTo(flameOrigin))

          pathLen = math.min(seg.length, ignitDist)

        } yield pathLen).getOrElse(0.0)
        
        println(s"maxPlantPath=$maxPlantPath  maxIncidentPath=$maxIncidentPath")

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
            var firstTestPt = true
            for (testPt <- testPoints) {
              var dryingFactor = 1.0
              var dryingTemp = site.temperature

              //compute the drying at testPt from preheating flames. NOTE that we have already popped the last
              //of the preheating flames off the vector because that level will provide the direct heating

              val preHeatingDrying =
                if (preHeatingFlames.size < 2) 1.0 // no drying
                else {
                  val dryingPerFlame = for {
                    phf <- locatedPreHeatingFlames
                    idt = calculateFlameIDT(phf.flame, locateFlameOrigin(phf.flame, iPt), testPt)
                    duration = phf.duration(context.preHeatingEndTime)
                  } yield math.max(0.0, 1.0 - duration / idt)

                  dryingPerFlame.product
                }

              dryingFactor *= preHeatingDrying

              //drying from the incident flames, but only if testPt is not already completely dry
              if (dryingFactor > 0) {
                val dryingFromIncidentFlames =
                  if (incidentFlames.isEmpty) 1.0 // no drying
                  else {
                    val N = math.min(timeStep - 1, incidentFlames.size)

                    val dryingPerFlame = for {
                      i <- 1 to N
                      flame = incidentFlames(i - 1)
                      idt = calculateFlameIDT(flame, locateFlameOrigin(flame, iPt), testPt)
                    } yield math.max(0.0, 1.0 - ModelSettings.ComputationTimeInterval / idt)

                    dryingPerFlame.product
                  }

                dryingFactor *= dryingFromIncidentFlames
              }

              //drying from the plant flames, but only if testPt is not already completely dry
              if (dryingFactor > 0) {
                val dryingFromPlantFlames =
                  if (plantFlames.isEmpty) 1.0 // no drying
                  else {
                    val dryingPerFlame = for {
                      flame <- plantFlames
                      // Use the plant flame origin in the IDT calculation
                      idt = calculateFlameIDT(flame, flame.origin, testPt)
                    } yield math.max(0.0, 1.0 - ModelSettings.ComputationTimeInterval / idt)

                    dryingPerFlame.product
                  }

                dryingFactor *= dryingFromPlantFlames
              }

              //compute temperatures at test Pt from incident flame and plant flame
              val incidentTemp = (for {
                flame <- incidentFlame
                origin = locateFlameOrigin(flame, iPt)
                t = flame.plumeTemperature(testPt.distanceTo(origin), site.temperature)
              } yield t).getOrElse(0.0)

              val plantTemp = (for {
                flame <- plantFlame
                t = flame.plumeTemperature(testPt.distanceTo(flame.origin), site.temperature)
              } yield t).getOrElse(0.0)

              println(s"plantTemp=$plantTemp incidentTemp=$incidentTemp")
              
              val maxTemp = math.max(incidentTemp, plantTemp)

              val idt = dryingFactor * calculateIDT(maxTemp)

              if (iPt == initialPoint && firstTestPt) {
                /*
                 * FIXME !!! Record pre-ignition data
                 *
                  resultBuilder.addPreIgnitionData( 
                      PreIgnitionData::incident(
                              incidentFlame.flameLength(), incidentFlame.depthIgnited(), 
                              distToIncidentFlame, dryingFactor, incidentTemp, idt) )
                 * 
                 */
              }

              //if ignition does not occur for testPt then break from loop over penetration steps
              if (idt > ModelSettings.ComputationTimeInterval || maxTemp < species.ignitionTemperature)
                PointLoop.break

              //if we get here ignition has occurred, so reset end pt and continue
              ePt = testPt

              firstTestPt = false
            } //end of loop over penetrations steps
          } // PointLoop.breakable
        }

        // Original comment --- set startTimeStep  when plant first ignites
        // (don't need to do this because the IgnitionResult class takes ignition time
        // from the first segment added)
        if (!ignition && ePt.distinctFrom(iPt)) {
          ignition = true
        }

        if (ignition) {
          if (!resultBuilder.hasIgnition) {
            println(s"adding first segment time=$timeStep start=$iPt end=$ePt")
            resultBuilder.addSegment(timeStep, iPt, ePt)
            plantFlames += newPlantFlame(modifiedWindSpeed)

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
              val n = resultBuilder.segments.size
              if (n < flameDuration) resultBuilder.segments.head.start
              else resultBuilder.segments(n - flameDuration).end
            }

            if (!Numerics.almostZero(maxIncidentPath) || !Numerics.almostZero(maxPlantPath) || segStart.distinctFrom(ePt)) {
              println(s"adding next segment time=$timeStep start=$segStart end=$ePt")
              resultBuilder.addSegment(timeStep, segStart, ePt)
              plantFlames += newPlantFlame(modifiedWindSpeed)
              // plantFlames.push_back(resultBuilder.flame(modifiedWindSpeed, slope()))

            } else {
              println(s"leaving time loop time=$timeStep")
              TimeStepLoop.break //from loop over time steps
            }
          }
          //reset ignition point
          iPt = ePt
          //increment counter if ignition has occurred
          safetyCounter += 1
        }
      } //end of loop over time steps

    } // TimeStepLoop.breakable

    println(s"finished ignition path")
    println

    return resultBuilder.toResult
  }

}