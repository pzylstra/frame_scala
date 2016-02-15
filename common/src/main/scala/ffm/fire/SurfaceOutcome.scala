package ffm.fire

trait SurfaceOutcome {
  /** Surface wind speed calculated during the simulation. */
  def windSpeed: Double
  
  /** Surface flames. */
  def flames: IndexedSeq[Flame]
}
