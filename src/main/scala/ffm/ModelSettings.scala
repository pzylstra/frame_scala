package ffm

object ModelSettings {

  val computationTimeInterval: Double = 1.0
  
 /** The maximum length of time for any single computation of an IgnitionPath. */
  val maxTime: Double = 25.0

  /** 
   *  The maximum number of time steps that will be used for the computation 
   *  of any single IgnitionPath.
   */
  val maxTimeSteps: Int = (maxTime / computationTimeInterval + 0.5).toInt

}