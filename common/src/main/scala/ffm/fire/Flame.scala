package ffm.fire

import ffm.geometry.Coord
import ffm.geometry.Ray

/**
 * Defines variables and methods of flame objects.
 */
trait Flame {

  /** Flame length (m). */
  def flameLength: Double
  
  /** Flame angle (radians). */
  def angle: Double
  
  /** Flame origin coordinates. */
  def origin: Coord
  
  /** Depth ignited (m). */
  def depthIgnited: Double
  
  /** Temperature (degrees C). */
  def deltaTemperature: Double

  /** Coordinates of flame tip. */  
  def tip: Coord

  /** A directed ray representing this flame. */
  val plume: Ray

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame. 
   */
  def plumeDeltaTemperature(dist: Double): Double

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame with the given ambient temperature.
   */
  def plumeTemperature(dist: Double, ambientTemp: Double): Double

  /** 
   * Determines the distance from origin at which targetTemp is achieved.
   * This is the inverse of plumeTemperature.
   * 
   * Returns None if the target temperature is higher than the flame temperature.
   */
  def distanceForTemperature(targetTemp: Double, ambientTemp: Double): Option[Double]
  
}

