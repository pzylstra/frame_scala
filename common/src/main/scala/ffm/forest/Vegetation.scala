package ffm.forest

trait Vegetation {
  
  /**
   * Collection of vegetation strata.
   */
  def strata: IndexedSeq[Stratum]
  
  /**
   * Vegetation strata indexed by stratum level.
   */
  def strataByLevel: Map[StratumLevel, Stratum]
  
  /**
   * Pair-wise overlaps between strata.
   */
  def overlaps: IndexedSeq[StratumOverlap]
  
  /**
   * Height-delimited layers of uniform vegetation from this vegetation's strata.
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
  def layers(includeCanopy: Boolean): IndexedSeq[VegetationLayer]
  
  /**
   * Test whether there is a vertical association between two strata.
   *
   * A vertical association exists if the lower of the two strata can exist under
   * the higher of the two. This can be optionally specified in the input parameters to the
   * model, otherwise it is determined by testing whether, on average, canopies of the
   * lower stratum sit below those of the upper stratum.
   */
  def isVerticalAssociation(s1: Stratum, s2: Stratum): Boolean  
}
