package ffm.fire

import ffm.forest.StratumLevel
import ffm.ModelSettings
import ffm.geometry.Coord
import ffm.forest.SpeciesComponent
import ffm.forest.Species
import ffm.geometry.Line
import ffm.geometry.Ray

object DefaultROS extends ROS {
  import StratumLevel._

  /** 
   * Calculates rates of spread for strata other than the canopy in an ignition run.
   */
  def calculateNonCanopy(res: FireModelRunResult): Map[StratumLevel, Double] = {
    val surfaceROS = res.surfaceOutcome.ros
    val slope = res.site.surface.slope

    val strataROSs = for {
      (level, pnf) <- res.pathsAndFlames
      if level != Canopy
      ros = level match {
          case NearSurface => calcNearSurface(res)
          case Elevated =>calcElevatedOrMidStorey(res, StratumLevel.Elevated)
          case MidStorey => calcElevatedOrMidStorey(res, StratumLevel.MidStorey)
          case _ => throw new RuntimeException("Unsupported level: " + level)
        }
    } yield (level -> ros)

    Map(StratumLevel.Surface -> surfaceROS) ++ strataROSs
  }

  
  /**
   * Calculates rate of spread for the canopy stratum.
   * 
   * The calculations are based on data over two ignition runs:
   * one with the canopy effect on wind included and the other without.
   */
  def calculateCanopy(resWithCanopyEffect: FireModelRunResult, resWithoutCanopyEffect: FireModelRunResult): Double = {
    val pnf =
      resWithoutCanopyEffect.pathsAndFlames.get(StratumLevel.Canopy) match {
        case Some(pnf) => pnf
        case None      => throw new RuntimeException("No Canopy stratum in fire model result")
      }

    val spreadsInStratum = pnf.stratumPaths exists { path =>
      path.speciesComponent.weighting > 0 && path.isSpreadingFire
    }
    
    if (spreadsInStratum) 
      math.min( speciesWeightedBasicROS(pnf.stratumPaths), crownFireROS(resWithCanopyEffect, resWithoutCanopyEffect) ) 
    else 
      0.0
  }

  
  private def calcNearSurface(res: FireModelRunResult): Double = {
    val pnf =
      res.pathsAndFlames.get(StratumLevel.NearSurface) match {
        case Some(pnf) => pnf
        case None      => throw new RuntimeException("No Near Surface stratum in fire model result")
      }

    if (pnf.plantPaths.isEmpty)
      0.0
    else {
      val plantSep = pnf.stratum.modelPlantSeparation

      // Calculate rates of spread from plant paths plus surface contribution
      val plantROSs = for {
        path <- pnf.plantPaths
        width = path.speciesComponent.species.crown.width
        occupancy = math.min(1.0, width / plantSep)
        ros = (occupancy * path.basicROS) + ((1.0 - occupancy) * res.surfaceOutcome.ros)
      } yield (path.speciesComponent, ros)

      // For any species with a stratum path, take the maximum
      // of the plant ROS and stratum ROS, otherwise just take the
      // plant ROS. Then calculate weighted values.
      //
      val weightedROSs = plantROSs map {
        case (spComp, plantROS) =>
          val spath = pnf.stratumPaths.find { path => path.speciesComponent.species.id == spComp.species.id }
          val ros = spath match {
            case Some(path) => math.max(plantROS, path.basicROS)
            case None       => plantROS
          }
          ros * spComp.weighting
      }

      // final result is sum of weighted species ROSs
      weightedROSs.sum
    }
  }

  
  private def calcElevatedOrMidStorey(res: FireModelRunResult, level: StratumLevel): Double = {
    val pnf =
      res.pathsAndFlames.get(level) match {
        case Some(pnf) => pnf
        case None      => throw new RuntimeException(s"No $level stratum in fire model result")
      }

    if (pnf.stratumPaths.isEmpty) {
      // no stratum spread
      0.0
    } else if (!pnf.stratumPaths.exists(path => path.basicROS > 0.0)) {
      // only stratum paths with zero rate (just in case)
      0.0
    } else {
      val lowFlameAngle = res.flameSummaries(level).flameAngle <= res.site.surface.slope

      val treatAsIndependent = lowFlameAngle && testStratumSpreadRate(pnf.stratumPaths)

      if (treatAsIndependent) {
        speciesWeightedBasicROS(pnf.stratumPaths)
      } else nonIndependentSpreadROS(pnf.stratumPaths)

    }
  }

  
  /*
   * Private helper to calculate weighted average of basic ROS for a 
   * set of ignition paths.
   */
  private def speciesWeightedBasicROS(paths: IndexedSeq[IgnitionPath]): Double =
    ( paths map { p => p.basicROS * p.speciesComponent.weighting } ).sum

  /*
   * Private helper method to test spread rate over all stratum paths 
   * against threshold value
   */
  private def testStratumSpreadRate(stratumPaths: IndexedSeq[IgnitionPath]): Boolean = {
    val fullPathROSs = stratumPaths map { path =>
      val n = path.segments.size
      if (n == ModelSettings.MaxIgnitionTimeSteps)
        (path.ros(n - 1) + path.ros(n - 2)) / 2 * path.speciesComponent.weighting
      else 0.0
    }

    fullPathROSs.sum >= ModelSettings.MinRateForStratumSpread
  }

  /*
   * Private helper to calculate ROS from a set of stratum ignition paths 
   * under non-independent fire spread
   */
  private def nonIndependentSpreadROS(stratumPaths: IndexedSeq[IgnitionPath]): Double = {
    val distancesAndTimes = for {
      path <- stratumPaths
      if path.hasIgnition && path.isSpreadingFire
      dist <- path.maxHorizontalRun
      time = (path.ignitionTimeStep + path.segments.size) * ModelSettings.ComputationTimeInterval
      wt = path.speciesComponent.weighting
    } yield (dist * wt, time * wt)

    val (distances, times) = distancesAndTimes.unzip
    val totalDist = distances.sum
    val totalTime = times.sum

    if (totalTime > 0) totalDist / totalTime else 0.0
  }

  /**
   * Calculates ROS for crown fire based on results from the ignition
   * runs with and without the canopy effect on wind included.
   * 
   * The calculations assume that if there is fire in the canopy stratum,
   * there must be fire in all lower strata.
   */
  private def crownFireROS(resWithCanopyEffect: FireModelRunResult, resWithoutCanopyEffect: FireModelRunResult): Double = {
    import IgnitionRunType._
    
    // stratum levels sorted from lowest to highest
    val levels = (resWithoutCanopyEffect.site.vegetation.strata map (_.level)).sortWith(_ < _)
    
    // Map of level -> next level up
    val nextLevelUp = (levels.init zip levels.tail).toMap
    
    // A case class to hold data for each stratum and make the
    // code a little easier to follow.
    //
    // Note: we leave retrieving stratum flame angles until later because
    // they come from another ignition run (with the canopy effect included)
    // and it makes the code a little clearer to keep them separate.
    
    case class Data(
        averageWidth: Double,
        averageBottom: Double,
        wtPlantIgTimeStep: Double,
        wtStratumIgTimeStep: Double,
        wtPlantMaxLenTimeStep: Double)
    
        
    // Collect required data from each stratum and put in 
    // a map to lookup by level
    val data = Map() ++ (for {
      level <- levels
      stratum = resWithCanopyEffect.site.vegetation.strataByLevel(level)
      
      avWidth = stratum.averageWidth
      avBottom = stratum.averageBottom
      
      pnf <- resWithCanopyEffect.pathsAndFlames.get(level)
      
      pig  = StratumPathsFlames.weightedIgnitionTimeStep(pnf, PlantRun)
      sig  = StratumPathsFlames.weightedIgnitionTimeStep(pnf, StratumRun)
      pmax = StratumPathsFlames.weightedTimeStepToMaxFlame(pnf, PlantRun)

    } yield (level -> Data(avWidth, avBottom, pig, sig, pmax)) )

    
    // Function to calculate a weighted flame origin for a given stratum
    // (other than the canopy)
    def spOrigin(level: StratumLevel, pnf: StratumPathsFlames): Coord = {
      require(level != StratumLevel.Canopy)
      
      val spp = pnf.plantPaths map (_.speciesComponent.species)
      val nextLevelTimeStep = data(nextLevelUp(level)).wtPlantIgTimeStep.toInt
      
      val wtOrigins = spp flatMap { sp =>
        pnf.pathForSpecies(sp, IgnitionRunType.StratumRun) match {
          case Some(path) if path.hasIgnition =>
            val i = math.min(path.segments.size, nextLevelTimeStep)
            Some( path.segments(i - 1).start multipliedBy path.speciesComponent.weighting )

          case _ =>
            // No stratum path with ignition for this species - use plant path
            // if present with ignition
            val path = pnf.pathForSpecies(sp, IgnitionRunType.PlantRun).get
            if (path.hasIgnition)
              Some( path.segments.last.start multipliedBy path.speciesComponent.weighting )
            else
              // no path with ignition, so this species makes no contribution
              // to the weighted flame origin
              None
        } 
      }

      // sum weighted coordinates to give the origin for the stratum
      wtOrigins.foldLeft(Coord.Origin)(_ add _)
    }

    // Calculate flame origin for strata other than
    // the canopy
    val origins = Map() ++ (for {
      level <- levels
      if level != StratumLevel.Canopy
      pnf <- resWithCanopyEffect.pathsAndFlames.get(level)
    } yield (level -> spOrigin(level, pnf)) )
    
    
    // Horizontal distances for strata (other than canopy) derived from
    // flame origins and average plant width
    val horizDistances = Map() ++ origins map { case (level, or) =>
      or.x + data(level).averageWidth
    }


    // Distances bridged from each lower stratum to the one above
    val slope = resWithCanopyEffect.site.surface.slope
    val bridgingDistances = for {
      (level, nextLevel) <- nextLevelUp
      y = data(nextLevel).averageBottom
      nextLowerEdge = Line( Coord(0, y), slope )

      // Note: flame angle is taken from the ignition
      // run in which the canopy effect on wind was included
      flameAngle = resWithCanopyEffect.flameSummaries(level).flameAngle
      
      
      plume = Ray( origins(level), flameAngle )
      crossing <- nextLowerEdge.intersection(plume)
      
      dist = crossing.x - origins(level).x
    } yield dist
    
    
    // Species weighted distances travelled in the canopy
    //
    val canopyPnf = resWithoutCanopyEffect.pathsAndFlames(StratumLevel.Canopy)
    val canopySpComps = canopyPnf.plantPaths map (_.speciesComponent)
      
    val canopyDistances = for {
      spc <- canopySpComps
      sp = spc.species

      path = canopyPnf.pathForSpecies(sp, IgnitionRunType.StratumRun) match {
          case Some(path) => path
          
          case None =>
            // No stratum path for this species - use plant path
            canopyPnf.pathForSpecies(sp, IgnitionRunType.PlantRun).get
        }
    
      x <- path.maxX
      dist = (x + sp.crown.width/2) * spc.weighting
      
    } yield dist
    
    // Sum all of the horizontal and bridging distances together with
    // the canopy species distances
    val horizDistSum = horizDistances.sum + bridgingDistances.sum + canopyDistances.sum
    
    // Calculate time when the fire spread in the canopy based
    // on both plant and stratum ignition paths
    val allCanopySpComps = canopyPnf.plantPaths map { p => p.speciesComponent }
    
    def numSpreadSegments(p: IgnitionPath): Int =
      ((0 until p.segments.size) count (i => p.ros(i) > ModelSettings.MinRateForStratumSpread))
    
    val canopyTimes = for {
      spComp <- allCanopySpComps
      sp = spComp.species
      wt = spComp.weighting
      
      plantPath <- canopyPnf.pathForSpecies(sp, IgnitionRunType.PlantRun)
      numSpreadingPlant = numSpreadSegments(plantPath)
      
      stratumPathOp = canopyPnf.pathForSpecies(sp, IgnitionRunType.StratumRun)
      numSpreadingStratum = 
        if (stratumPathOp.isDefined) numSpreadSegments(stratumPathOp.get)
        else 0
        
      weightedTime = (numSpreadingPlant + numSpreadingStratum) * ModelSettings.ComputationTimeInterval * wt
      
    } yield weightedTime
    
    
    // Aggregate ignition time over all strata
    val ignitionTimeSum = {
      data.values map { d =>
        val stepSum = d.wtPlantIgTimeStep + d.wtPlantMaxLenTimeStep + d.wtStratumIgTimeStep
        stepSum * ModelSettings.ComputationTimeInterval
      }
    }.sum


    // Final result
    horizDistSum / (ignitionTimeSum + canopyTimes.sum)
  }
  
}
