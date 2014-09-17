package ffm.forest

import ffm.util.ArgUtils

/**
 * Species with composition value (e.g. proportion within stratum).
 */
case class SpeciesComposition(species: Species, composition: Double) {
  
  ArgUtils.isPositive("composition", composition)
  
}