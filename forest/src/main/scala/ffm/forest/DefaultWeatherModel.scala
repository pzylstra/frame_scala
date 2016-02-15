package ffm.forest


/**
 * Weather model with constant temperature and wind speed.
 */
case class DefaultWeatherModel(temperature: Double, windSpeed: Double) extends WeatherModel
