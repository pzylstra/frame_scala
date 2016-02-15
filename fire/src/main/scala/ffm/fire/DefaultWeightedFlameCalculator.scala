package ffm.fire

import ffm.ModelSettings._
import ffm.geometry.Coord
import ffm.forest.DefaultSpeciesUtils
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel


object DefaultWeightedFlameCalculator extends WeightedFlames.Calculator {

  import WeightedFlames.{Result, EmptyResult, FlameParams}    

  /**
   * Derived weighted flame values from the given ignition paths.
   * 
   * Each path comes from a species within a single stratum. If `paths` is
   * empty (no ignitions occurred) an empty result object is returned.
   */
  def run(paths: IndexedSeq[IgnitionPath], level: StratumLevel, plantFlameModel: PlantFlameModel): Result =
    if (paths.isEmpty) EmptyResult
    else processPaths(paths, level, plantFlameModel)
    
  private def processPaths(paths: IndexedSeq[IgnitionPath], level: StratumLevel, plantFlameModel: PlantFlameModel): Result = {

    // Helper to recurse through paths, keeping track of weighted averages as we go
    //
    def iter(curRes: Result, curPaths: IndexedSeq[IgnitionPath]): Result = {
      if (curPaths.isEmpty) curRes
      else if (!curPaths.head.hasIgnition) iter(curRes, curPaths.tail)
      else {
        val path = curPaths.head
        val segments = path.segmentsByLengthAndTime
        
        val SpeciesComponent(species, wt) = path.speciesComponent

        val ignitionTime = path.ignitionTime * wt
        val timeToMaxLen = path.timeFromIgnitionToMaxLength * wt

        val t =
          if (DefaultSpeciesUtils.isGrass(species, level)) GrassFlameDeltaTemperature
          else MainFlameDeltaTemperature
          
        val flameParams = segments map { seg =>
          val length = plantFlameModel.flameLength(species, seg.length) * wt
          val depth = seg.length * wt
          val origin = seg.start multipliedBy wt
          val temp = length * t
          
          FlameParams(length, depth, origin, temp)
        }

        val res = Result(ignitionTime, timeToMaxLen, flameParams)

        iter(combine(curRes, res), curPaths.tail)
      }
    }

    // Launch the helper
    val res = iter(WeightedFlames.EmptyResult, paths)

    if (res.isEmpty) {
      // No weighted data: just return the empty result
      res
      
    } else {
      // Some weighted data: finalize the calculation of length-weighted 
      // temperatures and return the result
      val finalParams = (res.params) map { case FlameParams(len, dep, or, t) => FlameParams(len, dep, or, t / len) }        
      res.copy(params = finalParams)
    }
  }

  /**
   * Helper method to combine two result objects.
   */
  private def combine(res1: Result, res2: Result): Result = {
    import ffm.util.IndexedSeqUtils._
    
    if (res1.isEmpty && res2.isEmpty) EmptyResult
    else if (res1.isEmpty) res2
    else if (res2.isEmpty) res1
    else {
      val combinedParams = res1.params.combine(res2.params) { (fp1, fp2) =>
        FlameParams(
          length = fp1.length + fp2.length,
          depth = fp1.depth + fp2.depth,
          origin = fp1.origin add fp2.origin,
          temperature = fp1.temperature + fp2.temperature)
      }

      Result(
        res1.ignitionTime + res2.ignitionTime,
        res1.timeToLongestFlame + res2.timeToLongestFlame,
        combinedParams)
    }
  }
  
}
