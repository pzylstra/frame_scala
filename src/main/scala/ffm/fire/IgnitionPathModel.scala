package ffm.fire

import ffm.forest.SpeciesComponent
import ffm.geometry.Coord

trait IgnitionPathModel {
  def generatePath(context: IgnitionContext, plantFlameModel: PlantFlameModel)(speciesComponent: SpeciesComponent, intialPoint: Coord): IgnitionPath
}

