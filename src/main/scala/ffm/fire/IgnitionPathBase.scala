package ffm.fire

import ffm.geometry.Coord

/**
 * Defines elements to be used by both IgnitionPath (data object) and IgnitionPathBuilder.
 */
trait IgnitionPathBase {
  /** Starting position (on a canopy edge) for the ignition path simulation. */
  def initialPoint: Coord

  /** Time series of data on conditions prior to ignition. */
  def preIgnitionData: IndexedSeq[PreIgnitionData]

  /** Time series of ignited segments. */
  def segments: IndexedSeq[IgnitedSegment]

  /** Returns true if any pre-ignition data or ignited segments are stored. */
  def hasData: Boolean =
    !(preIgnitionData.isEmpty && segments.isEmpty)
  
  /** 
   * Returns true if ignition occurred, in which case there will be at 
   * least one ignited segment.
   */
  def hasIgnition: Boolean =
    !segments.isEmpty

  /** 
   * Returns the time step at which ignition occurred.
   * 
   *
   * Throws an exception if ignition did not occur.  
   */
  def ignitionTimeStep: Int =
    if (segments.isEmpty) throw new NoSuchElementException("No ignited segments: ignition time undefined")
    else segments.head.timeStep

 }
