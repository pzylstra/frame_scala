package ffm.fire

/**
 * 
 */
case class FireModelResult(run1: FireModelRunResult, run2: FireModelRunResult) {
  
  val hasSecondRun: Boolean =
    run2.stratumOutcomes.nonEmpty

}


/**
 * Holds ignition paths and flame series generated from plant and stratum
 * ignition runs, together with the surface fire parameters.
 */
case class FireModelRunResult (
    surfaceParams: SurfaceParams, 
    stratumOutcomes: IndexedSeq[StratumOutcome],
    combinedFlames: IndexedSeq[Flame]
    ) {
  
  /**
   * Creates an empty result object.
   */
  def this(surfaceParams: SurfaceParams) =
    this(surfaceParams, Vector.empty, Vector.empty)
  
  /**
   * The flame series with the largest flame for each stratum.
   */
  val flameSeriess: IndexedSeq[StratumFlameSeries] =
    stratumOutcomes.flatMap(_.largestFlameSeries)
    
  def withStratumOutcome(outcome: StratumOutcome) = copy(stratumOutcomes = stratumOutcomes :+ outcome)
  
  /** Sets the combined flames. */
  def withCombinedFlames(flames: IndexedSeq[Flame]) = copy(combinedFlames = flames)
  
}
