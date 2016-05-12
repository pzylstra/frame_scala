package ffm.fire

import ffm.forest.Stratum
import ffm.forest.Species

/**
 * Holds ignition paths and resulting flames for a single stratum.
 *
 * TODO: consider simplifying this and using separate data objects for plant and stratum runs.
 */
trait StratumPathsFlames {

  /** The vegetation stratum which this outcome pertains to. */
  def stratum: Stratum

  /** Ignition paths derived from plant ignition simulations. */
  def plantPaths: IndexedSeq[IgnitionPath]

  /** Flame series derived from plant ignition simulations. */
  def plantFlameSeries: Option[StratumFlameSeries]

  /** Ignition paths derived from stratum ignition simulations. */
  def stratumPaths: IndexedSeq[IgnitionPath]

  /** Flame series derived from stratum ignition simulations. */
  def stratumFlameSeries: Option[StratumFlameSeries]

  /** 
   * Retrieves an ignition path for a given species, if present.
   * 
   * The species is searched for using its integer ID value, so a
   * proxy species used for a stratum ignition run will match the
   * underlying plant species from which it was derived. 
   */
  def pathForSpecies(sp: Species, pathType: IgnitionRunType): Option[IgnitionPath] = pathType match {
    case IgnitionRunType.PlantRun => 
      plantPaths.find( _.speciesComponent.species.id == sp.id )
      
    case IgnitionRunType.StratumRun =>
      stratumPaths.find( _.speciesComponent.species.id == sp.id )
  }
}

/**
 * Provides utility methods.
 */
object StratumPathsFlames {

  /**
   * Select either plant flame series or stratum flame series from an outcome object.
   * 
   * This method takes a [[StratumOutcome]] object and a predicate function to choose
   * between them (e.g. on max flame length). If both plant flames and stratum flames
   * are present in the outcome, the predicate is used.  If only plant flames are present
   * they are returned. If no flames are present, an empty result (`None`) is returned.
   * The combination of stratum flames with no plant flames is not possible.
   */
  def selectMaxFlameSeries(
    spf: StratumPathsFlames,
    attrFn: (StratumFlameSeries) => Double): Option[StratumFlameSeries] = {

    spf.plantFlameSeries match {
      // plant flames present
      case Some(plantFS) =>
        spf.stratumFlameSeries match {
          case Some(stratumFS) =>
            // both flame series present, use provided function to choose one
            val maxFS = if (attrFn(plantFS) > attrFn(stratumFS)) plantFS else stratumFS
            Some(maxFS)

          // only plant flame series present
          case None =>
            Some(plantFS)
        }

      // no plant or stratum flames
      case None => None
    }
  }

  /**
   * Calculates the species-weighted ignition time for ignition paths 
   * of a given type.
   * 
   * @return weighted value in '''time steps''' (ie. not time units)
   */
  def weightedIgnitionTimeStep(spf: StratumPathsFlames, igType: IgnitionRunType): Double =
    weightedCalc(getPaths(spf, igType), _.ignitionTimeStep)

  /**
   * Calculates the species-weighted ignition time for ignition paths 
   * of a given type.
   * 
   * @return weighted value in '''time units''' (ie. not time steps)
   */
  def weightedIgnitionTime(spf: StratumPathsFlames, igType: IgnitionRunType): Double =
    weightedCalc(getPaths(spf, igType), _.ignitionTime)    
  
  /**
   * Calculates the species-weighted time step from ignition to
   * maximum flame length for ignition paths of a given type.
   * 
   * @return weighted value in '''time steps''' (ie. not time units)
   */
  def weightedTimeStepToMaxFlame(spf: StratumPathsFlames, igType: IgnitionRunType): Double =
    weightedCalc(getPaths(spf, igType), path => path.timeStepForMaxLength - path.ignitionTimeStep)

  /**
   * Calculates the species-weighted time from ignition to
   * maximum flame length for ignition paths of a given type.
   * 
   * @return weighted value in '''time units''' (ie. not time steps)
   */
  def weightedTimeToMaxFlame(spf: StratumPathsFlames, igType: IgnitionRunType): Double =
    weightedCalc(getPaths(spf, igType), _.timeFromIgnitionToMaxLength)
  
  
  /**
   * Gets paths of a given type from a [[StratumPathsFlames]] object.
   */
  def getPaths(spf: StratumPathsFlames, igType: IgnitionRunType): IndexedSeq[IgnitionPath] =
    igType match {
      case IgnitionRunType.PlantRun => spf.plantPaths
      case IgnitionRunType.StratumRun => spf.stratumPaths
    }

  /* 
   * Private helper to calculated species-weighted aggregate value
   * for an attribute retrieved by `attrFn` from a set of ignition
   * paths.
   */
  private def weightedCalc(paths: IndexedSeq[IgnitionPath], attrFn: (IgnitionPath) => Double): Double = {
    val ts = for {
      p <- paths
      if p.hasIgnition
    } yield attrFn(p) * p.speciesComponent.weighting
    
    ts.sum
  }

}
