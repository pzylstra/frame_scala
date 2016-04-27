package ffm.forest

import ffm.geometry.CrownPoly
import ffm.numerics.Numerics
import ffm.util.Options
import ffm.util.ArgUtils

/**
 * Companion object with a factory method to create Species instances
 * and check argument values.
 */
object DefaultSpecies {
  
  /**
   * Creates a new Species object.
   */
  def apply(
    name: String,
    crown: CrownPoly,
    liveLeafMoisture: Double,
    deadLeafMoisture: Double,
    propDead: Double,
    ignitionTemp: Double,
    leafForm: LeafForm,
    leafThickness: Double,
    leafWidth: Double,
    leafLength: Double,
    leafSeparation: Double,
    stemOrder: Double,
    clumpDiameter: Double,
    clumpSeparation: Double): Species = {

    require(!name.trim().isEmpty(), "species name is required")

    import ffm.util.ArgUtils._

    isNotNegative("liveLeafMoisture", liveLeafMoisture)
    isNotNegative("deadLeafMoisture", deadLeafMoisture)

    isProportion("propDead", propDead, allowZero = true)

    isPositive("ignitionTemp", ignitionTemp)

    isNotNegative("leafThickness", leafThickness)
    isNotNegative("leafWidth", leafWidth)
    isNotNegative("leafLength", leafLength)
    isNotNegative("leafSeparation", leafSeparation)

    isNotNegative("stemOrder", stemOrder)
    isNotNegative("clumpDiameter", clumpDiameter)
    isNotNegative("clumpSeparation", clumpSeparation)

    new PlantSpecies(name.trim(), crown,
      liveLeafMoisture, deadLeafMoisture, propDead,
      ignitionTemp,
      leafForm, leafThickness, leafWidth, leafLength, leafSeparation,
      stemOrder, clumpDiameter, clumpSeparation)
  }

  /**
   * The default class for a plant species. 
   * 
   * It is included here as a private class to force calling code to use the 
   * Species object apply method which does sanity checking of arguments.
   */
  private class PlantSpecies(
    val name: String,
    val crown: CrownPoly,
    val liveLeafMoisture: Double,
    val deadLeafMoisture: Double,
    val propDead: Double,
    val ignitionTemperature: Double,
    val leafForm: LeafForm,
    val leafThickness: Double,
    val leafWidth: Double,
    val leafLength: Double,
    val leafSeparation: Double,
    val stemOrder: Double,
    val clumpDiameter: Double,
    val clumpSeparation: Double) extends Species {

    val propLive = 1.0 - propDead

    /** Approximate leaf area (single side) */
    val leafArea = leafWidth * leafLength / 2.0

    val leafMoisture = propLive * liveLeafMoisture + propDead * deadLeafMoisture
     
    /**
     * Ignitability coefficient.
     */
    val ignitabilityCoef: Double = {
      val leafFactor = leafForm match {
        case LeafForm.Round => 4.0
        case _ => 2.0
      }
      
      100 * leafMoisture * leafThickness * 1000 / leafFactor
    }
    
    /**
     * Estimated leaf density.
     */
    val leavesPerClump = 0.88 * math.pow(clumpDiameter * stemOrder / leafSeparation, 1.18)

    /**
     * Estimated leaf area index.
     */
    val leafAreaIndex = {
      val clumpVolume = 4.0 / 3.0 * math.Pi * math.pow((clumpDiameter + clumpSeparation) / 2, 3)
      val groundArea = math.Pi * math.pow(crown.width / 2, 2)

      leafArea * leavesPerClump * crown.volume / clumpVolume / groundArea
    }

    override def toString = s"Species($name)"
  }

}

