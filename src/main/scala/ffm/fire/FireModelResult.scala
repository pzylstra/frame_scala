package ffm.fire

/**
 * Holds ignition paths and flame series generated from plant and stratum
 * ignition runs, together with the surface fire parameters.
 */
class FireModelResult(val surfaceParams: SurfaceParams, val paths: IndexedSeq[IgnitionPath], val flameSeriess: IndexedSeq[StratumFlameSeries]) {
  /** Creates an empty instance. */
  def this(surfaceParams: SurfaceParams) = this(surfaceParams, Vector.empty, Vector.empty)

  /** Adds ignition paths. */
  def add(newPaths: IndexedSeq[IgnitionPath]) = new FireModelResult(surfaceParams, paths ++ newPaths, flameSeriess)

  /** Adds a flame series. */
  def add(newSeries: StratumFlameSeries) = new FireModelResult(surfaceParams, paths, flameSeriess :+ newSeries)
}
