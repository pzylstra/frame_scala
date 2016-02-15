package ffm.forest

/**
 * Query / helper methods related to species attributes.
 */
trait SpeciesUtils {
  
  /** Tests if a species in a given stratum should be treated as a grass. */
  def isGrass(sp: Species, level: StratumLevel): Boolean
  
}