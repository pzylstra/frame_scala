package ffm.forest

import ffm.numerics.{Numerics, RoundedDoubleSortedSet}

/**
 * Represents a stand of vegetation, consisting of strata and
 * optional data on their overlaps.
 */
class DefaultVegetation(val strata: IndexedSeq[Stratum], val overlaps: IndexedSeq[StratumOverlap]) extends Vegetation {
  
  /** Strata indexed by StatumLevel. */
  val strataByLevel: Map[StratumLevel, Stratum] =
    Map() ++ (strata map (s => (s.level -> s) ))

  /**
   * Identifies height-delimited layers of uniform vegetation from this vegetation's strata.
   *
   * For example, given a stand with three strata:
   * - Canopy 5 - 20m
   * - Mid-storey 2 - 8m
   * - Surface 0 - 1m
   *
   * The vegetation bands would be:
   * - 8 - 20m canopy
   * - 5 - 8m mid-storey, canopy
   * - 2 - 5m mid-storey
   * - 1 - 2m empty layer
   * - 0 - 1m surface
   *
   * If no stratum has a lower height at ground level, an empty ground layer
   * should be included.
   *
   * Layers are returned in descending order of height.
   *
   * @param includeCanopy whether to include the canopy (if any) in the returned layers.
   */
  def layers(includeCanopy: Boolean): IndexedSeq[VegetationLayer] = {
    if (strata.isEmpty) Vector.empty
    else {
      /*
       * Get the average lower and upper height of each stratum and 
       * combine them into a list of unique heights in descending order
       */
      val set = RoundedDoubleSortedSet(Numerics.DistanceTolerance)
      for (stratum <- strata if includeCanopy || stratum.level != StratumLevel.Canopy) {
        set.add(stratum.averageBottom)
        set.add(stratum.averageTop)
      }
      set.add(0.0) // in case there is no stratum reaching ground level
      val hts = set.toVector.reverse

      /*
       * Examine bands defined by successive height pairs and, for each band,
       * determine the stratum levels within it (if any) and return the results
       * as a Layer object. 
       */
      val layers = for {
        (upperHt, lowerHt) <- hts.zip(hts.tail)

        midHt = (lowerHt + upperHt) / 2
        levels = for {
          stratum <- strata
          if includeCanopy || stratum.level != StratumLevel.Canopy
          if midHt > stratum.averageBottom && midHt < stratum.averageTop
        } yield (stratum.level)

      } yield VegetationLayer(lowerHt, upperHt, levels)

      layers
    }
  }

  /**
   * Tests whether there is a vertical association between two strata.
   *
   * A vertical association exists if the lower of the two strata can exist under
   * the higher of the two. This can be optionally specified in the input parameters to the
   * model, otherwise it is determined by testing whether, on average, canopies of the
   * lower stratum sit below those of the upper stratum.
   */
  def isVerticalAssociation(s1: Stratum, s2: Stratum): Boolean = {
    import StratumOverlapType._

    val (lower, upper) =
      if (s1.level < s2.level) (s1, s2)
      else (s2, s1)
      
    val ovtype =
      overlaps find (ov => ov.lower == lower && ov.upper == upper) match {
        case Some(ov) => ov.overlapType
        case None => Undefined
      }

    ovtype match {
      case Overlapping => true
      case NotOverlapping => false
      case Undefined => lower.averageTop <= upper.averageBottom
    }
  }
  
}

object DefaultVegetation {
  def apply(strata: Seq[Stratum], overlaps: Seq[StratumOverlap]): Vegetation =
    new DefaultVegetation(strata.toVector, overlaps.toVector)
}