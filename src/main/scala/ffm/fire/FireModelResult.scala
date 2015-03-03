package ffm.fire

/**
 * Holds ignition paths and flame series generated from plant and stratum
 * ignition runs, together with the surface fire parameters.
 */
case class FireModelResult(
    surfaceParams: SurfaceParams, 
    paths: IndexedSeq[IgnitionPath], 
    flameSeriess: IndexedSeq[StratumFlameSeries],
    combinedFlames: IndexedSeq[Flame]
    ) {
  
  def this(surfaceParams: SurfaceParams) =
    this(surfaceParams, Vector.empty, Vector.empty, Vector.empty)
    
  /** Adds ignition paths. */
  def add(newPaths: IndexedSeq[IgnitionPath]) = copy(paths = paths ++ newPaths)

  /** Adds a flame series. */
  def add(newSeries: StratumFlameSeries) = copy(flameSeriess = flameSeriess :+ newSeries)
  
  /** Sets the combined flames. */
  def withCombinedFlames(flames: IndexedSeq[Flame]) = copy(combinedFlames = flames)
}
