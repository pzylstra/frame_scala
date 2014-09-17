package ffm.forest

import ffm.numerics.Numerics
import ffm.ModelSettings
import scala.collection.mutable.TreeSet
import ffm.numerics.RoundedDoubleSortedSet
import scala.collection.mutable.ArrayBuffer

/**
 * Models the effect of a site's vegetation on incident wind speed.
 */
object VegetationWindModel {

  /**
   * Finds the wind speed at the given height within a site considering the damping effect of 
   * the site's vegetation layers.
   * 
   * If the target height is above the highest vegetation stratum being considered, the incident
   * wind speed at the site is returned unmodified.
   */
  def windSpeedAtHeight(height: Double, site: Site, includeCanopy: Boolean): Double = {
    if (site.windSpeed <= 0) 0.0
    else if (site.strata.isEmpty) site.windSpeed
    else {
      val workingHeight = math.max(height, ModelSettings.MinHeightForWindModel)
      val layers = site.vegetationLayers(includeCanopy)
      
      val topLayerHeight = (layers.map(_.upper)).max
      
      if (workingHeight > topLayerHeight) site.windSpeed
      else doWindSpeedCalculation(height=workingHeight, layers, site)
    }
  }
  
  
  private def doWindSpeedCalculation(
      height: Double, 
      layers: Vector[VegetationLayer], 
      site: Site): Double = {

    // Map of leaf area index values indexed by stratum level.
    // We might not need all of them but it is quick and easy to get them.
    val levelLAIs = Map() ++ site.strata.map( stratum => (stratum.level -> stratum.leafAreaIndex))
    
    // Function to iterate through the layers, progressively modifying the wind speed
    // until the layer containing the target height is reached.
    def iter(curLayers: Vector[VegetationLayer], curWindSpeed: Double, refHt: Double): Double = {
      assert(!curLayers.isEmpty, s"Failed to find level for height=$height") 
      
      val curLayer = layers.head
      val fn = (z: Double, gamma: Double) => curWindSpeed * math.exp(gamma * (z / refHt - 1.0))
      
      if (curLayer.isEmptyLayer) {
        if (height >= curLayer.lower) curWindSpeed
        else iter(layers.tail, curWindSpeed, curLayer.lower)
      
      } else {  // layer with vegetation
        val sumLAI = (curLayer.levels map (levelLAIs)).sum
        val gamma = 1.785 * math.pow(sumLAI, 0.372)
        if (height >= curLayer.lower) fn(height, gamma)
        else iter(layers.tail, fn(curLayer.lower, gamma), curLayer.lower)
      }
    }
    
    // Launch the iter function to do the calculation
    iter(layers, site.windSpeed, refHt=layers.head.upper)
  }

}