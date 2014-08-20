package ffm.forest


class Stratum(val level: Stratum.Level, 
    val species: Vector[Species], 
    val plantSep: Double) {

}


object Stratum {
  sealed trait Level
  
  case object Unknown extends Level
  case object Surface extends Level
  case object NearSurface extends Level
  case object Elevated  extends Level
  case object MidStorey extends Level
  case object Canopy extends Level
}