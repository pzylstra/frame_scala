package ffm.forest

trait VegetationWindModel {

  /** Calculate wind reduction factor. */
  def windReductionFactor(site: Site): Double
  
  /** Calculate surface wind speed with or without canopy. */
  def surfaceWindSpeed(site: Site, includeCanopy: Boolean): Double
  
  /**
   * Calculate the wind speed at the given height considering the damping effect of 
   * the vegetation.
   * 
   * If the target height is above the highest vegetation stratum being considered, the incident
   * wind speed at the site is returned unmodified.
   */
  def windSpeedAtHeight(height: Double, site: Site, includeCanopy: Boolean): Double  
  
}
