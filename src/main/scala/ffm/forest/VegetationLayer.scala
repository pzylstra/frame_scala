package ffm.forest

/**
 * A VegetationLayer represents a horizontal band of uniform forest composition.
 * 
 * @param lower lower height (m)
 * @param upper upper height (m)
 * @param levels forest strata making up the layer (may be empty)
 */
case class VegetationLayer(lower: Double, upper: Double, levels: Vector[StratumLevel]) {
  
  require(upper > lower, "Layer top must be greater than bottom")
  
  /**
   * Creates an empty VegetationLayer.
   */
  def this(lower: Double, upper: Double) = this(lower, upper, Vector.empty)
  
  /**
   * Checks if this layer is empty.
   */
  def isEmptyLayer = levels.isEmpty
  
}
