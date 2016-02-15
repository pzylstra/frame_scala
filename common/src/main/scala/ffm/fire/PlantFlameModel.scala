package ffm.fire

import ffm.forest.Species

/**
 * Methods to be implemented by classes which calculate species-specific ignition 
 * and flame attributes.
 */
trait PlantFlameModel {
  /**
   * Ignition delay time for a species at the given temperature.
   */
  def ignitionDelayTime(species: Species, temperature: Double): Double
  
  /**
   * Flame duration for a species (seconds).
   */
  def flameDuration(species: Species): Double
  
  /**
   * Leaf flame length for a species.
   */
  def leafFlameLength(species: Species): Double

  /**
   * Flame length for a species given the length of the ignited
   * segment within the crown.
   */
  def flameLength(species: Species, lengthIgnitedSeg: Double): Double
}
