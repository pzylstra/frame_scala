package ffm.fire

/**
 * 
 */
case class FireModelResult(run1: FireModelRunResult, run2: FireModelRunResult) {
  
  val hasSecondRun: Boolean =
    run2.paths.nonEmpty

}


/**
 * Holds ignition paths and flame series generated from plant and stratum
 * ignition runs, together with the surface fire parameters.
 */
case class FireModelRunResult (
    surfaceParams: SurfaceParams, 
    paths: IndexedSeq[IgnitionPath], 
    flameSeriess: IndexedSeq[StratumFlameSeries],
    combinedFlames: IndexedSeq[Flame]
    ) {
  
  /**
   * Creates a new result object with empty vectors for paths,
   * flameSeriess and combinedFlames.
   */
  def this(surfaceParams: SurfaceParams) =
    this(surfaceParams, Vector.empty, Vector.empty, Vector.empty)
    
  /** Adds ignition paths. */
  def add(newPaths: IndexedSeq[IgnitionPath]) = copy(paths = paths ++ newPaths)

  /** Adds a flame series. */
  def add(newSeries: StratumFlameSeries) = copy(flameSeriess = flameSeriess :+ newSeries)
  
  /** Sets the combined flames. */
  def withCombinedFlames(flames: IndexedSeq[Flame]) = copy(combinedFlames = flames)
}
