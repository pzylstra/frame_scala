package ffm.forest

trait WeatherModel {
  /** Temperature (degrees C). */
  def temperature: Double
  
  /** Wind speed (m/s). */
  def windSpeed: Double
}
