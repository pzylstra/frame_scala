package ffm.forest

import ffm.geometry.Coord

/**
 * Represents a site in the modelling landscape having:
 * - an integer identifier
 * - a location
 * - surface characteristics
 * - zero or more vegetation strata
 * - weather
 */
trait Site {
  def id: Long
  def location: Coord
  def surface: Surface
  def vegetation: Vegetation
  def weather: WeatherModel
  
  /** Gets the slope (shortcut for site.surface.slope). */
  def slope = surface.slope

  /** Gets the ambient temperature (shortcut for site.weather.temperature). */
  def temperature = weather.temperature

  /** Gets the incident windSpeed (shortcut for site.weather.windSpeed). */
  def windSpeed = weather.windSpeed
}

/**
 * Class for single-site model.
 */
case class SingleSite(surface: Surface, vegetation: Vegetation, weather: WeatherModel) extends Site {
  val id = 1L
  val location = Coord(0, 0)
}
