package ffm.forest

import ffm.ModelSettings
import ffm.numerics._

/**
 * Holds parameters for the site surface.
 * 
 * @param slope surface slope (radians)
 * @param deadFuelMoistureProp dead fuel moisture as a proportion
 * @param fuelLoad fuel load (kg / sq m)
 * @param meanFuelDiameter (m)
 * @param meanFinenessLeaves (m)
 */
case class Surface(
  slope: Double,
  deadFuelMoistureProp: Double,
  fuelLoad: Double,
  meanFuelDiameter: Double,
  meanFinenessLeaves: Double) {

  require(slope >= -math.Pi && slope <= math.Pi, s"Invalid slope ($slope)")

  import ffm.util.ArgUtils._

  isProportion("deadFuelMoistureProp", deadFuelMoistureProp, allowZero = true)
  isNotNegative("fuelLoad", fuelLoad)
  isNotNegative("meanFuelDiameter", meanFuelDiameter)
  isNotNegative("meanFinenessLeaves", meanFinenessLeaves)
}
