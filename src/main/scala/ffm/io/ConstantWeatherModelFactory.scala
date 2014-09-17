package ffm.io

import scala.util.Try

import ffm.forest.ConstantWeatherModel
import ffm.forest.WeatherModel

object ConstantWeatherModelFactory {

  import ExpressionSyntax._
  import FactoryItem._
  
  val items = List(
    item("incident wind speed", "windSpeed"),
    item("air temperature", "temperature")
  )
  
  def create(modelDef: ModelDef): Try[WeatherModel] = {
    for {
      vas <- Try( new ValueAssignments(modelDef.params, items) )
      weatherModel <- Try( new ConstantWeatherModel(temperature=vas.dval("temperature"), windSpeed=vas.dval("windSpeed")) )
    } yield weatherModel
  }
    
}
