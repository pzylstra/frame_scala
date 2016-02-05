package ffm.fire

import ffm.forest.SpeciesComponent
import ffm.geometry.Coord

/**
 * Defines the sub-model for the occurrence and progression of ignition within a plant crown.
 * 
 * We anticipate having alternative implementations of this sub-model, e.g. for comparison of algorithms
 * or to tailor the flammability model for particular domains.
 */
trait IgnitionPathModel {
  /**
   * Models an ignition path in a plant crown.
   * 
   * @param context object providing environmental and fire variables
   * @param plantFlameModel flame model to use for species-specific ignition and flame calculations
   * @param speciesComponent species for which ignition is being modelled
   * @param initialPoint the point on the species crown edge from which the ignition path may commence
   * 
   * @return the modelled ignition path (may be empty)
   */
  def generatePath(context: IgnitionContext, plantFlameModel: PlantFlameModel)(speciesComponent: SpeciesComponent, intialPoint: Coord): IgnitionPath
}

