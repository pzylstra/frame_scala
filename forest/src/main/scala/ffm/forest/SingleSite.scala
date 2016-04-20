package ffm.forest

import ffm.geometry.Coord

/**
 * Class for single-site model.
 */
case class SingleSite(surface: Surface, vegetation: Vegetation, weather: WeatherModel, context: SiteContext) extends Site {
  val id = 1L
  val location = Coord(0, 0)
}
