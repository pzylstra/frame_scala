package ffm.fire

import ffm.forest.Site
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.geometry.Coord

trait IgnitionPathModel {

  def generatePath(
    runType: IgnitionRunType,
    context: IgnitionContext,
    intialPoint: Coord): IgnitionPath

}

sealed trait IgnitionRunType
object IgnitionRunType {
  case object PlantRun extends IgnitionRunType
  case object StratumRun extends IgnitionRunType
}

case class IgnitionContext(
  site: Site,
  stratumLevel: StratumLevel,
  speciesComponent: SpeciesComponent,
  preHeatingFlames: IndexedSeq[PreHeatingFlame],
  incidentFlames: IndexedSeq[Flame],
  preHeatingEndTime: Double,
  canopyHeatingDistance: Double,
  stratumWindSpeed: Double)

  
