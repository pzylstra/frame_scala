package ffm.fire

import ffm.forest.Site

/**
 * Holds combined results of ignition runs.
 */
class DefaultFireModelResult(
    val site: Site,
    val resWithCanopyEffect: FireModelRunResult,
    val resWithoutCanopyEffect: FireModelRunResult) extends FireModelResult {
  
  val hasSecondRun: Boolean =
    resWithoutCanopyEffect.pathsAndFlames.nonEmpty
    
  val canopyROS: Double = 
    if (hasSecondRun) DefaultROS.calculateCanopy(resWithCanopyEffect, resWithoutCanopyEffect)
    else 0.0

}

