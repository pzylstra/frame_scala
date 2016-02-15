package ffm.fire

import ffm.ModelSettings
import ffm.numerics.Stats
import ffm.geometry.Coord
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel

/**
 * Implements [[IgnitionPath]] and provides an algorithm for basic rate of spread.
 */
class DefaultIgnitionPath(
    val stratumLevel: StratumLevel,
    val speciesComponent: SpeciesComponent,
    val initialPoint: Coord,
    val preIgnitionData: IndexedSeq[PreIgnitionData],
    val segments: IndexedSeq[IgnitedSegment]) extends IgnitionPath {
  
  val basicROS: Double =
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
