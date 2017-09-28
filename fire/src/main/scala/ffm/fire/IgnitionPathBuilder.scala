package ffm.fire

import scala.collection.mutable.ArrayBuffer
import ffm.forest.SpeciesComponent
import ffm.geometry.Coord
import ffm.forest.StratumLevel

/**
 * Used to progressively record results during an ignition path run and
 * then provide them as an immutable IgnitionPath object.
 */
trait IgnitionPathBuilder {
  /** Records pre-heating flame drying prior to ignition. */
  def recordPreHeatingFlameDrying(
    time: Int,
    flame: PreHeatingFlame,
    distanceToFlame: Double,
    dryingFactor: Double,
    dryingTemperature: Double,
    duration: Double): Unit

  /** Records incident flame drying prior to ignition. */
  def recordIncidentFlameDrying(
    time: Int,
    flame: Flame,
    distanceToFlame: Double,
    dryingFactor: Double,
    dryingTemperature: Double,
    ignitionDelayTime: Double): Unit

  /** Adds a new segment. */
  def addSegment(timeStep: Int, start: Coord, end: Coord, flameLength: Double)

  /** The number of segments added so far. */
  def numSegments: Int
  
  /** Returns the segments added so far. */
  def segments: IndexedSeq[IgnitedSegment]
  
  /** Checks if ignition has occurred. */
  def hasIgnition: Boolean
  
  /** 
   * Returns the time step at which ignition occurred.
   * 
   * This should only be called if `hasIgnition` returns `true`.
   */
  def ignitionTimeStep: Int

  /** The first segment. 
    *
    * Throws an exception if no segments have been added.  
    */
  def head: IgnitedSegment

  /** The last segment. 
   *  
   * Throws an exception if no segments have been added.  
   */
  def last: IgnitedSegment

  /**
   * Returns an immutable IgnitionPath object based on the data held
   * by this builder.
   */
  def toIgnitionPath: IgnitionPath
}

/**
 * Companion object for the IgnitionPathBuilder trait.
 *
 * Usage:
 * {{{
 * // At the beginning of an ignition path simulation
 * val pathBuilder = IgnitionPathBuilder(ignitionContext, speciesComponent, initialPoint)
 *
 * // During the simulation
 * pathBuilder.addSegment(timeStep, startPoint, endPoint)
 *
 * // At the end of the simulation
 * pathBuilder.toIgnitionPath
 * }}}
 */
object IgnitionPathBuilder {
  def apply(context: IgnitionContext, speciesComponent: SpeciesComponent, initialPoint: Coord): IgnitionPathBuilder =
    new Builder(context, speciesComponent, initialPoint)

  private class Builder(val context: IgnitionContext, val speciesComponent: SpeciesComponent, val initialPoint: Coord) extends IgnitionPathBuilder {
    private val segmentBuffer = ArrayBuffer.empty[IgnitedSegment]
    private val preIgnitionBuffer = ArrayBuffer.empty[PreIgnitionData]

    /**
     *  Records pre-heating flame drying prior to ignition.
     */
    def recordPreHeatingFlameDrying(
      time: Int,
      flame: PreHeatingFlame,
      distanceToFlame: Double,
      dryingFactor: Double,
      dryingTemperature: Double,
      duration: Double) {

      assert(!hasIgnition, "Attempting to add pre-ignition data after ignition has been recorded")
      preIgnitionBuffer += new PreHeatingDrying(time, flame, distanceToFlame, dryingFactor, dryingTemperature, duration)
    }

    /** Records incident flame drying prior to ignition. */
    def recordIncidentFlameDrying(
      time: Int,
      flame: Flame,
      distanceToFlame: Double,
      dryingFactor: Double,
      dryingTemperature: Double,
      ignitionDelayTime: Double) {

      assert(!hasIgnition, "Attempting to add pre-ignition data after ignition has been recorded")
      preIgnitionBuffer += new IncidentDrying(time, flame, distanceToFlame, dryingFactor, dryingTemperature, ignitionDelayTime)
    }

    def preIgnitionData = preIgnitionBuffer.toVector
    def segments = segmentBuffer.toVector

    def addSegment(timeStep: Int, start: Coord, end: Coord, flameLength: Double): Unit = {
      if (!hasIgnition)
        require(start.almostEq(initialPoint))
      else
        require(timeStep == segmentBuffer.last.timeStep + 1,
            s"Time step for ignited segment ($timeStep) should be later than previous time (${segmentBuffer.last.timeStep})")
            
      segmentBuffer += new IgnitedSegment(timeStep, start, end, flameLength)
    }

    def numSegments = segmentBuffer.size
    
    def hasIgnition = numSegments > 0
    
    def ignitionTimeStep =
      if (segments.isEmpty) throw new UnsupportedOperationException("No ignited segments: ignition time undefined")
      else segments.head.timeStep

    def head = segmentBuffer.head

    def last = segmentBuffer.last

    def toIgnitionPath = new DefaultIgnitionPath(context, speciesComponent, initialPoint, preIgnitionData, segments)

  }

}

