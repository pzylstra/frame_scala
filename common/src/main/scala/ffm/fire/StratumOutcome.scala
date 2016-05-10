package ffm.fire

import ffm.forest.Stratum

/**
 * Represents the results of an ignition simulation for a particular stratum.
 *
 * TODO: consider simplifying this and using separate data objects for plant and stratum runs.
 */
trait StratumOutcome {

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

}

/**
 * Provides utility methods.
 */
object StratumOutcome {

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
    so: StratumOutcome,
    attrFn: (StratumFlameSeries) => Double): Option[StratumFlameSeries] = {

    so.plantFlameSeries match {
      // plant flames present
      case Some(plantFS) =>
        so.stratumFlameSeries match {
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

}
