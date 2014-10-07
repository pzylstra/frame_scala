package ffm.fire

import ffm.ModelSettings
import ffm.forest.SpeciesComponent
import ffm.geometry.Coord
import ffm.numerics.Numerics.DoubleEx

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
  
  lazy val segmentsLargeToSmall: IndexedSeq[IgnitedSegment] =
    segments.sortBy(_.length).reverse

  lazy val maxSegmentLength: Double =
    if (!hasIgnition) 0.0
    else segmentsLargeToSmall.head.length 
    
  lazy val maxDryingTemperature: Double =
    preIgnitionData.foldLeft(Double.MinValue) { (t, pid) => t max pid.dryingTemperature }
  
  lazy val timeFromIgnitionToMaxLength: Double =
    if (!hasIgnition) 0.0
    else {
      val maxLen = segmentsLargeToSmall.head.length
      
      // There may be more than one segment with max length and they are not sorted
      // on time
      val timeSteps = segments.filter(_.length.almostEq(maxLen)) map (_.timeStep)
      val timeStepsSinceIgnition = timeSteps.min - ignitionTimeStep
      timeStepsSinceIgnition * ModelSettings.ComputationTimeInterval 
    }
}

