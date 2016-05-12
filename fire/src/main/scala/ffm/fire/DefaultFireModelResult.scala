package ffm.fire

import ffm.forest.Site

/**
 * Aggregates results from separate runs of a fire model, with and without a canopy layer.
 */
class DefaultFireModelResult(
    val site: Site,
    val run1: FireModelRunResult,
    val run2: FireModelRunResult) extends FireModelResult {
  
  val hasSecondRun: Boolean =
    run2.pathsAndFlames.nonEmpty

}

