package ffm.forest

import ffm.util.ArgUtils

/**
 * A species with a weighting value (e.g. proportion within stratum).
 */
case class SpeciesComponent(species: Species, weighting: Double) {
  
  ArgUtils.isPositive("weighting", weighting)
  
}