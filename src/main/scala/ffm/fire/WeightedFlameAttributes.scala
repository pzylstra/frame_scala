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
  /** Weighted average time from ignition to maximum flame length (zero if no data). */
  def timeToLongestFlame: Double
  
  /** Weighted average flame length per ignited segment over all paths. */
  def flameLengths: Seq[Double]
  
  /** Weighted average flameDepths per ignited segment over all paths. */
  def flameDepths: Seq[Double]
  
  /** Weighted average flame origin per ignited segment over all paths. */
  def origins: Seq[Coord]
  
  /** 
   *  Weighted average flame temperature per ignited segment over all paths.
   * 
   *  The weighting is based both on species composition and flame length. 
   */
  def temperatures: Seq[Double]
  
  /**
   * Size of the sequence attributes.
   */
  def size: Int = flameLengths.size
  
  /**
   * Returns true if there is no attribute data.
   */
  def isEmpty: Boolean = flameLengths.isEmpty
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
    val timeToLongestFlame = 0.0
    val flameLengths = Vector.empty[Double]
    val flameDepths = Vector.empty[Double]
    val origins = Vector.empty[Coord]
    val temperatures = Vector.empty[Double]
  }
  
  case class NonEmpty(
    timeToLongestFlame: Double,
    flameLengths: Seq[Double],
    flameDepths: Seq[Double],
    origins: Seq[Coord],
    temperatures: Seq[Double]) extends WeightedFlameAttributes {

    require(flameDepths.size == size && origins.size == size && temperatures.size == size,
      "All attribute sequences must be the same length")
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

        val timeToMaxLen = path.timeFromIgnitionToMaxLength * wt

        val lengths = segments.map(seg => plantFlameModel.flameLength(species, seg.length) * wt)
        val depths = segments.map(_.length * wt)
        val origins = segments.map(_.start.times(wt))

        val t =
          if (Species.isGrass(species, stratumLevel)) GrassFlameDeltaTemperature
          else MainFlameDeltaTemperature

        val temps = lengths.map(_ * t)

        val attrs = NonEmpty(timeToMaxLen, lengths, depths, origins, temps)

        iter(combine(curAttrs, attrs), curPaths.tail)
      }
    }

    val init = Empty
    val attrs = iter(init, paths)

    // If we have data, finalize the calculation of length-weighted temperatures
    attrs match {
      case attrs: NonEmpty =>
        val finalTemps = (attrs.temperatures zip attrs.flameLengths) map { case (t, len) => t / len }
        attrs.copy(temperatures = finalTemps)
        
      case _ =>
        attrs
    }
  }

  /** 
   * Combines data from two weighted attribute objects into a single object. 
   */
  private def combine(attr1: WeightedFlameAttributes, attr2: WeightedFlameAttributes) = {
    import ffm.util.IndexedSeqUtils._

    NonEmpty(
      attr1.timeToLongestFlame + attr2.timeToLongestFlame,
      attr1.flameLengths.combine(attr2.flameLengths, _ + _),
      attr1.flameDepths.combine(attr2.flameDepths, _ + _),
      attr1.origins.combine(attr2.origins, (cthis, cthat) => cthis.add(cthat)),
      attr1.temperatures.combine(attr2.temperatures, _ + _))
  }

}
