package ffm.forest

/**
 * Weather model with constant temperature and wind speed.
 */
case class ConstantWeatherModel(temperature: Double, windSpeed: Double) extends WeatherModel
