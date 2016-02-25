package ffm.fire

import ffm.geometry.Coord
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.ModelSettings
import ffm.numerics.Stats
import ffm.numerics.Numerics

trait IgnitionPath {
  /** The species for which ignition was modelled, and its weighting within the stratum. */
  def speciesComponent: SpeciesComponent
  
  /** 
   * Context data (including site, stratum level, flames) which applied to the ignition 
   * simulation. 
   */
  def context: IgnitionContext
  
  /** The initial point on the edge of the crown for the ignition simulation. */
  def initialPoint: Coord

  /** Time series of data on conditions prior to ignition. */
  def preIgnitionData: IndexedSeq[PreIgnitionData]

  /** Time series of ignited segments. */
  def segments: IndexedSeq[IgnitedSegment]

  /** Checks if any data (pre-ignition data or ignited segments) exist. */
  def hasData: Boolean =
    !(preIgnitionData.isEmpty && segments.isEmpty)
  
  /** Checks if ignition occurred. */
  def hasIgnition: Boolean =
    !segments.isEmpty

  /** 
   * Gets the time step of ignition.
   * 
   * Throws an exception if ignition did not occur.
   */
  def ignitionTimeStep: Int =
    if (hasIgnition) segments.head.timeStep
    else throw new UnsupportedOperationException("Ignition did not occur")
    
  /** 
   * Gets the time of ignition.
   * 
   * This is just the ignition time step multiplied by the computation time interval.
   * 
   * Throws an exception if ignition did not occur.
   */
  def ignitionTime: Double = 
    ignitionTimeStep * ModelSettings.ComputationTimeInterval

  /** 
   * Returns the sequence of ignited segments ordered firstly by length (decreasing)
   * and then by time step (increasing).
   */
  def segmentsByLengthAndTime: IndexedSeq[IgnitedSegment] =
    segments.sortWith { (s1, s2) =>  
      if (Numerics.Distance.almostEq(s1.length, s2.length)) {
        s1.timeStep > s2.timeStep
      } else { 
        Numerics.Distance.lt(s1.length, s2.length)
      }
    }.reverse
    
  /** 
   * Maximum ignited segment length. 
   * 
   * Returns 0.0 if ignition did not occur.
   */
  def maxSegmentLength: Double =
    if (!hasIgnition) 0.0
    else segmentsByLengthAndTime.head.length 

  /**
   * Finds the earliest time step with a segment which has maximum length.
   * 
   * Throws an exception if ignition did not occur.
   */
  def timeStepForMaxLength =
    if (hasIgnition) segmentsByLengthAndTime.head.timeStep 
    else throw new UnsupportedOperationException("Ignition did not occur")

  /**
   * Finds the maximum recorded pre-ignition drying temperature.
   */
  def maxDryingTemperature: Double =
    preIgnitionData.foldLeft(Double.MinValue) { (t, pid) => t max pid.dryingTemperature }
  
  /**
   * Time elapsed between ignition and when maximum segment length first occurs.
   */
  def timeFromIgnitionToMaxLength: Double =
    if (!hasIgnition) 0.0
    else (timeStepForMaxLength - ignitionTimeStep) * ModelSettings.ComputationTimeInterval 
    
  /**
   * Mean rate of spread calculated from segments for which the individual
   * rate of spread is greater than [[ModelSettings.MinRateForStratumSpread]].
   * 
   * Implementing classes must provide this method.
   */
  def basicROS: Double
}

