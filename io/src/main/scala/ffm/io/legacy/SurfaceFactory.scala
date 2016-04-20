package ffm.io.legacy

import scala.util.Try
import ffm.forest.Surface
import ffm.util.Units

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
      // input slope is degrees
      slope = Units.convert("deg", "rad", vas.dval("slope") ),
      deadFuelMoistureProp = vas.dval("deadFuelMoistureProp"),
      // input fuel load is tonnes per hectare - convert to kg / sq m
      fuelLoad = Units.convert("t/ha", "kg/m2", vas.dval("fuelLoad") ),  
      meanFuelDiameter = vas.dval("meanFuelDiameter"),
      meanFinenessLeaves = vas.dval("meanFinenessLeaves"))
  }
}
