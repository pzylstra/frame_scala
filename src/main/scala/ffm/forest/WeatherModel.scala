package ffm.forest

trait WeatherModel {
  def temperature: Double
  def windSpeed: Double
}

/**
 * Weather model with constant temperature and wind speed.
 */
case class ConstantWeatherModel(temperature: Double, windSpeed: Double) extends WeatherModel
