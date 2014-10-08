package ffm.fire

import ffm.forest.Site
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.geometry.Coord

trait IgnitionPathModel {
  def generatePath(context: IgnitionContext, plantFlameModel: PlantFlameModel)(speciesComponent: SpeciesComponent, intialPoint: Coord): IgnitionPath
}

sealed trait IgnitionRunType
object IgnitionRunType {
  case object PlantRun extends IgnitionRunType
  case object StratumRun extends IgnitionRunType
}

case class IgnitionContext(
  runType: IgnitionRunType,
  site: Site,
  stratumLevel: StratumLevel,
  preHeatingFlames: IndexedSeq[PreHeatingFlame],
  incidentFlames: IndexedSeq[Flame],
  preHeatingEndTime: Double,
  canopyHeatingDistance: Double,
  stratumWindSpeed: Double)

  
