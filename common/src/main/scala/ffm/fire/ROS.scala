package ffm.fire

import ffm.forest.StratumLevel

/**
 * Defines a calculator to derive rate of spread values for strata.
 * 
 * Calculations for strata other than the canopy are based on data from
 * an individual run. For the canopy, the calculations combine data 
 * from two runs, one with and the other without the canopy stratum 
 * included in the ignition simulation.
 * 
 */
trait ROS {
  
  /** 
   * Calculates rates of spread for strata other than the canopy in an ignition run.
   */
  def calculateNonCanopy(res: FireModelRunResult): Map[StratumLevel, Double]
  
  /**
   * Calculates rate of spread for the canopy stratum.
   * 
   * The calculations are based on data over two ignition runs:
   * one with the canopy effect on wind included and the other without.
   */
  def calculateCanopy(
      resWithCanopyEffect: FireModelRunResult,
      resWithoutCanopyEffect: FireModelRunResult): Double 
      
}
