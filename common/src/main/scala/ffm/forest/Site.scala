package ffm.forest

import ffm.geometry.Coord

/**
 * Represents a site in the modelling landscape having:
 * - an integer identifier
 * - a location
 * - surface characteristics
 * - vegetation with zero or more strata
 * - weather
 */
trait Site {
  def id: Long
  def location: Coord
  def surface: Surface
  def vegetation: Vegetation
  def weather: WeatherModel
}
