package ffm.io

import ffm.forest.Surface
import scala.util.Try

object SurfaceFactory {

  import ExpressionSyntax._
  import FactoryItem._

  val items = List(
    item("slope", "slope"),
    item("surface dead fuel moisture content", "deadFuelMoistureProp"),
    item("fuel load tonnes per hectare", "fuelLoad"),
    item("mean fuel diameter", "meanFuelDiameter"),
    item("mean fineness leaves", "meanFinenessLeaves"))

  def create(modelDef: ModelDef): Try[Surface] = {
    for {
      vas <- Try(new ValueAssignments(modelDef.params, items))
      surface <- Try(buildSurface(vas))
    } yield surface
  }

  private def buildSurface(vas: ValueAssignments): Surface = {
    Surface(
      slope = vas.dval("slope"),
      deadFuelMoistureProp = vas.dval("deadFuelMoistureProp"),
      fuelLoad = vas.dval("fuelLoad"),
      meanFuelDiameter = vas.dval("meanFuelDiameter"),
      meanFinenessLeaves = vas.dval("meanFinenessLeaves"))
  }
}