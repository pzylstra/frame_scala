package ffm.fire

import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.forest.Vegetation

/**
 * Holds data for a single run of a [[FireModel]].
 */
case class DefaultFireModelRunResult(
    site: Site,
    surfaceOutcome: SurfaceOutcome,
    stratumOutcomes: IndexedSeq[StratumOutcome],
    combinedFlames: IndexedSeq[Flame]) extends FireModelRunResult {
}


/**
 * A builder class to progressively add result data before retrieving
 * a final [[FireModelRunResult]] object.
 * 
 * Some ordering is enforced by the `add` methods: surface data must be
 * added first, then stratum outcomes (if any), then combined flames (if any).
 */
class DefaultFireModelRunResultBuilder(site: Site) {

  private object EmptySurfaceOutcome extends SurfaceOutcome {
    val windSpeed = 0.0
    val flames = Vector.empty
    val flameSummary = StratumFlameSummary(StratumLevel.Surface, 0.0, 0.0, 0.0)
  }

  private val EmptyResult = DefaultFireModelRunResult(site, EmptySurfaceOutcome, Vector.empty, Vector.empty)

  private var result = EmptyResult

  def addSurfaceOutcome(surf: SurfaceOutcome): Unit = {
    require(result.surfaceOutcome == EmptySurfaceOutcome, 
        "Adding a surface outcome when prior data have been set")
    
    result = result.copy(surfaceOutcome = surf)
  }

  def addStratumOutcome(strat: StratumOutcome): Unit = {
    require(result.combinedFlames.isEmpty, 
        "Adding a stratum outcome after combined flames")
    
    result = result.copy(stratumOutcomes = result.stratumOutcomes :+ strat)
  }

  def addCombinedFlames(flames: IndexedSeq[Flame]): Unit = {
    // If the set of flames is not empty, the existing result object should have
    // one or more stratum outcomes already.
    require(flames.isEmpty || !result.stratumOutcomes.isEmpty,
      "Adding combined flames to a result object with no stratum outcomes")

    result = result.copy(combinedFlames = flames)
  }

  /**
   * The flame series with the largest flame for each stratum.
   */
  def largestFlameSeriesPerStratum(): IndexedSeq[StratumFlameSeries] =
    result.stratumOutcomes.flatMap { outcome =>
      outcome.selectFlameSeries((fs1, fs2) => if (fs1.maxFlameLength > fs2.maxFlameLength) fs1 else fs2)
    }

  /**
   * Return the result object in its current state.
   * 
   * Note: each call to this method returns a new immutable result object.
   */
  def toResult(): FireModelRunResult = result

}

