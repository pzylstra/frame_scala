package ffm.fire

import ffm.ModelSettings
import ffm.forest.SpeciesComponent
import ffm.geometry.Coord
import ffm.numerics.Numerics

trait IgnitionPathBase {
  def initialPoint: Coord

  def preIgnitionData: IndexedSeq[PreIgnitionData]

  def segments: IndexedSeq[IgnitedSegment]

  def hasData: Boolean =
    !(preIgnitionData.isEmpty && segments.isEmpty)
  
  def hasIgnition: Boolean =
    !segments.isEmpty

  def ignitionTimeStep: Int =
    if (segments.isEmpty) throw new NoSuchElementException("No ignited segments: ignition time undefined")
    else segments.head.timeStep
    
  def lastTimeStep: Int =
    if (segments.isEmpty) throw new NoSuchElementException("No ignited segments: last time step undefined")
    else segments.last.timeStep

 }

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

