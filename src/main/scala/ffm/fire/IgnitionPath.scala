package ffm.fire

import ffm.ModelSettings
import ffm.forest.SpeciesComponent
import ffm.numerics.Numerics
import ffm.numerics.Stats


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
      if (Numerics.Distance.almostEq(s1.length, s2.length)) {
        s1.timeStep > s2.timeStep
      } else { 
        Numerics.Distance.lt(s1.length, s2.length)
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
    
  /**
   * Mean rate of spread calculated from segments for which the individual
   * rate of spread is greater than [[ModelSettings.MinRateForStratumSpread]].
   */
  lazy val basicROS: Double =
    if (!hasIgnition) 0.0
    else {
      val xs = segments.head.start.x +: segments.map(_.end.x)
      
      val xpairs = xs zip xs.tail
      val rates = xpairs map { case (x0, x1) => (x1 - x0) / ModelSettings.ComputationTimeInterval }
      val spreaders = rates filter (_ > ModelSettings.MinRateForStratumSpread)
      
      if (spreaders.isEmpty) 0.0
      else Stats.mean(spreaders)
    }
      
}

