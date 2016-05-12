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
    val context: IgnitionContext,
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
  
   def ros(segIndex: Int): Double = {
     if (segIndex >= segments.size) 0.0
     else if (segIndex == 0) (segments(0).end.x - segments(0).start.x) / ModelSettings.ComputationTimeInterval
     else (segments(segIndex).end.x - segments(segIndex - 1).end.x) / ModelSettings.ComputationTimeInterval
   }
   
   val isSpreadingFire: Boolean = {
     // flag segments which have at least min rate of spread required
     val flags = (0 until segments.size) map (i => if (ros(i) >= ModelSettings.MinRateForStratumSpread) 1 else 0)
     
     flags.sum >= ModelSettings.MinTimeStepsForStratumSpread
   }
 }
