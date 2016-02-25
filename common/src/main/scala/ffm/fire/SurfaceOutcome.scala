package ffm.fire

/**
 * Holds flame and wind speed data for the surface.
 * 
 * TODO: can this be merged into the other stratum result classes ?
 */
trait SurfaceOutcome {
  /** Surface wind speed calculated during the simulation. */
  def windSpeed: Double
  
  /** Surface flames. */
  def flames: IndexedSeq[Flame]
  
  /** Summary attributes for flames. */
  def flameSummary: StratumFlameSummary
}
