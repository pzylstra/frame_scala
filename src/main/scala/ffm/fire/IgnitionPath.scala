package ffm.fire

import ffm.ModelSettings
import ffm.forest.SpeciesComponent
import ffm.geometry.Coord
import ffm.numerics.Numerics

/**
 * Defines elements to be used by both IgnitionPaths (data objects) and IgnitionPathBuilder.
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
   * @throws NoSuchElementException if there was no ignition.
   */
  def ignitionTimeStep: Int =
    if (segments.isEmpty) throw new NoSuchElementException("No ignited segments: ignition time undefined")
    else segments.head.timeStep

 }

/**
 * Extends IgnitionPathBase with elements for IgnitionPath data objects.
 * 
 * Most of the elements defined here are lazy vals because they depend on
 * [[ffm.fire.IgnitionPathBase.segments]] being defined.
 */
trait IgnitionPath extends IgnitionPathBase {
  def context: IgnitionContext
  
  def speciesComponent: SpeciesComponent
  
  def ignitionTime: Double = 
    ignitionTimeStep * ModelSettings.ComputationTimeInterval 
  
  /** 
   * Returns the sequence of ignited segments ordered firstly by length (decreasing)
   * and then by time step (increasing).
   */
  lazy val segmentsByLengthAndTime: IndexedSeq[IgnitedSegment] =
    segments.sortWith { (s1, s2) =>  
      if (Numerics.almostEq(s1.length, s2.length)) {
        s1.timeStep > s2.timeStep
      } else { 
        Numerics.lt(s1.length, s2.length)
      }
    }.reverse
    
  /** 
   * Maximum ignited segment length. 
   */
  lazy val maxSegmentLength: Double =
    if (!hasIgnition) 0.0
    else segmentsByLengthAndTime.head.length 

  /**
   * Earliest time step of a segment with maximum length.
   */
  lazy val timeStepForMaxLength =
    if (hasIgnition) segmentsByLengthAndTime.head.timeStep 
    else throw new NoSuchElementException("No ignited segments: timeStepForMaxFlame undefined")

  /**
   * Maximum recorded pre-ignition drying temperature.
   */
  lazy val maxDryingTemperature: Double =
    preIgnitionData.foldLeft(Double.MinValue) { (t, pid) => t max pid.dryingTemperature }
  
  /**
   * Time elapsed between ignition and when maximum segment length first occurs.
   */
  lazy val timeFromIgnitionToMaxLength: Double =
    if (!hasIgnition) 0.0
    else (timeStepForMaxLength - ignitionTimeStep) * ModelSettings.ComputationTimeInterval 

}

