package ffm.fire

import ffm.ModelSettings._
import ffm.forest.LeafForm
import ffm.forest.Species
import ffm.numerics.Numerics


/**
 * Default implementation of PlantFlameModel.
 */
object DefaultPlantFlameModel extends PlantFlameModel {

  /**
   * Calculates ignition delay time for a species at the given temperature.
   */
  def ignitionDelayTime(species: Species, temperature: Double): Double = {
    val leafFactor = species.leafForm match {
      case LeafForm.Round => 4.0
      case _ => 2.0
    }

    val m = 100 * species.leafMoisture * species.leafThickness * 1000 / leafFactor

    100168.23 * math.pow(temperature, -2.11) * m + 6018087.86 * math.pow(temperature, -2.39)
  }

  /**
   * Calculates flame duration for a species (seconds).
   */
  def flameDuration(species: Species): Double =
    math.max(
      1.361 * species.leafWidth * species.leafThickness * 1.0e6 + 1.095,
      ComputationTimeInterval)

  /**
   * Calculates leaf flame length for a species.
   */
  def leafFlameLength(species: Species): Double = {
    val sqRootArea = math.sqrt(species.leafArea)
    val cubeRootArea = math.cbrt(species.leafArea)
    if (species.leafMoisture < (17.5 * cubeRootArea - 52.5 * sqRootArea - 0.0027) / 0.277)
      1.75 * cubeRootArea - 0.0277 * species.leafMoisture - 0.00027
    else
      5.25 * sqRootArea
  }

/**
   * Calculates flame length for a species given the length of the ignited
   * segment within the crown.
   *
   * Merged leaf flame length model (Zylstra thesis Eq 5.76).
   * We use the average of the number of clumps that will be
   * traversed by the ignited segment (Eq 5.63).
   * 
   * Lateral merging in plants uses a horizontal slice of the clump
   *
   * Does merging of leaf flame lengths but does not do lateral
   * merging of plant flames.
   */
  def flameLength(species: Species, lengthIgnitedSeg: Double): Double = {
    if (Numerics.Distance.almostZero(lengthIgnitedSeg)) 0.0
    else {
      val numLeaves = species.leavesPerClump * math.min((lengthIgnitedSeg / (species.clumpDiameter + species.clumpSeparation)), 2 * species.leafSeparation )
      val term1 = math.pow(leafFlameLength(species) * math.pow(numLeaves, 0.4) + lengthIgnitedSeg, 4.0)
      val term2 = math.pow(lengthIgnitedSeg, 4.0)
      math.max(lengthIgnitedSeg, math.pow(term1 + term2, 0.25))
    }
  }
}

