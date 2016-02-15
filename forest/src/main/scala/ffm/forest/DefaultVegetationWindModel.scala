package ffm.forest

import ffm.ModelSettings

/**
 * Models the effect of a site's vegetation on incident wind speed.
 */
object DefaultVegetationWindModel extends VegetationWindModel {
  
  /**
   * Calculates the Wind Reduction Factor.
   * 
   * WRF = incident wind speed / wind speed at 1.5m
   */
  def windReductionFactor(site: Site): Double =
    if (site.weather.windSpeed <= 0) 1.0
    else site.weather.windSpeed / windSpeedAtHeight(1.5, site, includeCanopy = true)

  /**
   * Calculates surface wind speed.
   * 
   * Takes into account whether the site contains a near-surface stratum or not.
   */
  def surfaceWindSpeed(site: Site, includeCanopy: Boolean): Double = {
    import StratumLevel._

    val surfaceHeight =
      if (site.vegetation.strataByLevel.isDefinedAt(NearSurface))
        math.max(ModelSettings.MinHeightForWindModel, site.vegetation.strataByLevel(NearSurface).averageMidHeight)
      else
        ModelSettings.MinHeightForWindModel

    windSpeedAtHeight(surfaceHeight, site, includeCanopy)
  }
  
  /**
   * Finds the wind speed at the given height within a site considering the damping effect of 
   * the site's vegetation layers.
   * 
   * If the target height is above the highest vegetation stratum being considered, the incident
   * wind speed at the site is returned unmodified.
   */
  def windSpeedAtHeight(height: Double, site: Site, includeCanopy: Boolean): Double = {
    if (site.weather.windSpeed <= 0) 0.0
    else if (site.vegetation.strata.isEmpty) site.weather.windSpeed
    else {
      val workingHeight = math.max(height, ModelSettings.MinHeightForWindModel)
      val layers = site.vegetation.layers(includeCanopy)
      
      val topLayerHeight = (layers.map(_.upper)).max
      
      if (workingHeight > topLayerHeight) site.weather.windSpeed
      else doWindSpeedCalculation(height=workingHeight, layers, site)
    }
  }
  
  
  private def doWindSpeedCalculation(
      height: Double, 
      layers: IndexedSeq[VegetationLayer], 
      site: Site): Double = {

    // Map of leaf area index values indexed by stratum level.
    // We might not need all of them but it is quick and easy to get them.
    val levelLAIs = Map() ++ site.vegetation.strata.map( stratum => (stratum.level -> stratum.leafAreaIndex))
    
    // Function to iterate through the layers, progressively modifying the wind speed
    // until the layer containing the target height is reached.
    def iter(curLayers: IndexedSeq[VegetationLayer], curWindSpeed: Double, refHt: Double): Double = {
      assert(!curLayers.isEmpty, s"Failed to find level for height=$height") 
      
      val curLayer = curLayers.head
      val fn = (z: Double, gamma: Double) => curWindSpeed * math.exp(gamma * (z / refHt - 1.0))
      
      if (curLayer.isEmptyLayer) {
        if (height >= curLayer.lower) curWindSpeed
        else iter(curLayers.tail, curWindSpeed, curLayer.lower)
      
      } else {  // layer with vegetation
        val sumLAI = (curLayer.levels map (levelLAIs)).sum
        val gamma = 1.785 * math.pow(sumLAI, 0.372)
        if (height >= curLayer.lower) fn(height, gamma)
        else iter(curLayers.tail, fn(curLayer.lower, gamma), curLayer.lower)
      }
    }
    
    // Launch the iter function to do the calculation
    iter(layers, site.weather.windSpeed, refHt=layers.head.upper)
  }

}