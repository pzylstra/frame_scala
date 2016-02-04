package ffm.fire

import ffm.ModelSettings._
import ffm.geometry.Coord
import ffm.forest.{ Species, SpeciesComponent }



/**
 * Holds weighted average attributes extracted from a collection of
 * IgnitionPaths.
 * 
 * The sequence attributes such as flameLengths represent weighted averages
 * over the ignited segments from all paths. Their length is equal to the maximum
 * number of segments in any path.
 */
trait WeightedFlameAttributes {
  /** Weighted average time of ignition (zero if no data). */
  def ignitionTime: Double
  
  /** Weighted average time from ignition to maximum flame length (zero if no data). */
  def timeToLongestFlame: Double
  
  /** Weighted flame parameters (length, depth, origin and temperature). */
  def flameParams: IndexedSeq[FlameParams]
  
  /**
   * Size of the sequence attributes.
   */
  def size: Int
  
  /**
   * Returns true if there is no attribute data.
   */
  def isEmpty: Boolean
}

/**
 * Companion object to the WeightedFlameAttributes trait used to create instances from
 * a collection of IgnitionPaths.
 *
 * Usage: 
 * {{{
 * val attr: WeightedFlameAttributes = WeightedFlameAttributes(paths)
 * }}}
 * 
 */
object WeightedFlameAttributes {
  
  object Empty extends WeightedFlameAttributes {
    val ignitionTime = 0.0
    val timeToLongestFlame = 0.0
    val flameParams = Vector.empty[FlameParams]
    val size = 0
    val isEmpty = true
  }

  case class NonEmpty(
    ignitionTime: Double,
    timeToLongestFlame: Double,
    flameParams: IndexedSeq[FlameParams]) extends WeightedFlameAttributes {

    val size = flameParams.size

    require(size > 0, "Bummer - looks like bad programming. No flame parameters found.")

    val isEmpty = false
  }

  /**
   * Returns weighted flame attributes from the given IgnitionPaths using
   * the given [[ffm.fire.PlantFlameModel]].
   */
  def apply(plantFlameModel: PlantFlameModel)(paths: IndexedSeq[IgnitionPath]): WeightedFlameAttributes = {
    if (paths.isEmpty) Empty
    else processPaths(paths, plantFlameModel)
  }
  
  /**
   * Processes a non-empty sequence of IgnitionPaths and calculates weighted attributes.
   */
  private def processPaths(paths: IndexedSeq[IgnitionPath], plantFlameModel: PlantFlameModel): WeightedFlameAttributes = {
    /*
     * Ensure that all paths are from the same stratum level.
     */
    val levels = paths.map(_.context.stratumLevel).toSet
    require(levels.size == 1, "Expected all paths from the same level but got: " + levels.mkString(", "))

    val stratumLevel = levels.head

    // Helper to recurse through paths, keeping track of weighted averages as we go
    def iter(curAttrs: WeightedFlameAttributes, curPaths: IndexedSeq[IgnitionPath]): WeightedFlameAttributes = {
      if (curPaths.isEmpty) curAttrs
      else if (!curPaths.head.hasIgnition) iter(curAttrs, curPaths.tail)
      else {
        val path = curPaths.head
        val segments = path.segmentsByLengthAndTime 
        val SpeciesComponent(species, wt) = path.speciesComponent

        val ignitionTime = path.ignitionTime * wt
        val timeToMaxLen = path.timeFromIgnitionToMaxLength * wt

        val t =
          if (Species.isGrass(species, stratumLevel)) GrassFlameDeltaTemperature
          else MainFlameDeltaTemperature
          
        val flameParams = segments map { seg =>
          val length = plantFlameModel.flameLength(species, seg.length) * wt
          val depth = seg.length * wt
          val origin = seg.start multipliedBy wt
          val temp = length * t
          
          FlameParams(length, depth, origin, temp)
        }

        val attrs = NonEmpty(ignitionTime, timeToMaxLen, flameParams)

        iter(combine(curAttrs, attrs), curPaths.tail)
      }
    }

    // Launch the helper
    val attrs = iter(Empty, paths)

    // If we have data, finalize the calculation of length-weighted temperatures
    attrs match {
      case attrs: NonEmpty =>
        val finalParams = (attrs.flameParams) map { case FlameParams(len, dep, or, t) => FlameParams(len, dep, or, t / len) }        
        attrs.copy(flameParams = finalParams)
        
      case _ =>
        attrs
    }
  }

  /**
   * Combines data from two weighted attribute objects into a single object.
   */
  private def combine(attr1: WeightedFlameAttributes, attr2: WeightedFlameAttributes): WeightedFlameAttributes = {
    import ffm.util.IndexedSeqUtils._
    
    if (attr1.isEmpty && attr2.isEmpty) Empty
    else if (attr1.isEmpty) attr2
    else if (attr2.isEmpty) attr1
    else {
      val combinedParams = attr1.flameParams.combine(attr2.flameParams) { (fp1, fp2) =>
        FlameParams(
          length = fp1.length + fp2.length,
          depth = fp1.depth + fp2.depth,
          origin = fp1.origin add fp2.origin,
          temperature = fp1.temperature + fp2.temperature)
      }

      NonEmpty(
        attr1.ignitionTime + attr2.ignitionTime,
        attr1.timeToLongestFlame + attr2.timeToLongestFlame,
        combinedParams)
    }
  }
  
}
