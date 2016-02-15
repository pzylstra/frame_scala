package ffm.forest

import ffm.numerics.Numerics

object DefaultSpeciesUtils extends SpeciesUtils {
  
  /**
   * Tests if a species should be treated as a grass.
   * 
   * TODO: danger ! hidden magic numbers - replace with something explicit and editable
   */
  def isGrass(sp: Species, stratumLevel: StratumLevel): Boolean =
    stratumLevel == StratumLevel.NearSurface &&
      Numerics.Default.geq(sp.propDead, 0.5) &&
      sp.leafThickness < 0.00035
      
}
