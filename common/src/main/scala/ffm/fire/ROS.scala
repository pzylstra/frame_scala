package ffm.fire

import ffm.forest.StratumLevel

/**
 * Defines a calculator to derive rate of spread values for strata
 * based on the results of an ignition run.
 */
trait ROS {
  def calculate(res: FireModelRunResult): Map[StratumLevel, Double]
}
