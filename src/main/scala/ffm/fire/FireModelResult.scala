package ffm.fire

/**
 * Holds ignition paths and flame series generated from plant and stratum
 * ignition runs.
 */
class FireModelResult(val paths: IndexedSeq[IgnitionPath], val flameSeriess: IndexedSeq[StratumFlameSeries]) {
  /** Creates an empty instance. */
  def this() = this(Vector.empty, Vector.empty)

  /** Adds ignition paths. */
  def add(newPaths: IndexedSeq[IgnitionPath]) = new FireModelResult(paths ++ newPaths, flameSeriess)

  /** Adds a flame series. */
  def add(newSeries: StratumFlameSeries) = new FireModelResult(paths, flameSeriess :+ newSeries)
}
