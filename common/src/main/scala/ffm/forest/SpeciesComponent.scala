package ffm.forest

import ffm.util.ArgUtils

/**
 * Pairs a species with an associated weighting value (e.g. proportion within stratum).
 */
case class SpeciesComponent(species: Species, weighting: Double) {
  
  ArgUtils.isPositive("weighting", weighting)
  
}
