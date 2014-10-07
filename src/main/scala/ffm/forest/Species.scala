package ffm.forest

import ffm.ModelSettings
import ffm.geometry.CrownPoly
import ffm.numerics.Numerics
import ffm.util.Options

trait Species {
  def name: String
  def crown: CrownPoly
  
  def liveLeafMoisture: Double
  def deadLeafMoisture: Double
  def propDead: Double
  
  def leafForm: LeafForm
  def leafThickness: Double
  def leafWidth: Double
  def leafLength: Double
  def leafArea: Double
  def leafMoisture: Double
  def leafSeparation: Double
  def leavesPerClump: Double
  def leafAreaIndex: Double
  
  def stemOrder: Double
  def clumpDiameter: Double
  def clumpSeparation: Double

  def ignitionTemperature: Double
}

/**
 * Companion object with a factory method to create Species instances
 * and check argument values.
 */
object Species {
  
  /**
   * Creates a new Species object.
   */
  def apply(
    name: String,
    crown: CrownPoly,
    liveLeafMoisture: Double,
    deadLeafMoisture: Double,
    propDead: Double,
    propSilicaFreeAsh: Option[Double] = None,
    ignitionTemp: Option[Double] = None,
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

    require(Options.atLeast(1, propSilicaFreeAsh, ignitionTemp),
      s"species $name has neither silica free ash proportion or ignition temp provided")

    propSilicaFreeAsh match {
      case Some(p) => isProportion("propSilicaFreeAsh", p, allowZero = false)
      case None => // no value provided
    }

    ignitionTemp match {
      case Some(t) => isPositive("ignitionTemp", t)
      case None => // no value provided
    }

    isNotNegative("leafThickness", leafThickness)
    isNotNegative("leafWidth", leafWidth)
    isNotNegative("leafLength", leafLength)
    isNotNegative("leafSeparation", leafSeparation)

    isNotNegative("stemOrder", stemOrder)
    isNotNegative("clumpDiameter", clumpDiameter)
    isNotNegative("clumpSeparation", clumpSeparation)

    new PlantSpecies(name.trim(), crown,
      liveLeafMoisture, deadLeafMoisture, propDead,
      propSilicaFreeAsh, ignitionTemp,
      leafForm, leafThickness, leafWidth, leafLength, leafSeparation,
      stemOrder, clumpDiameter, clumpSeparation)
  }

  /**
   * Tests if a species should be treated as a grass.
   */
  def isGrass(sp: Species, stratumLevel: StratumLevel): Boolean =
    stratumLevel == StratumLevel.NearSurface &&
      sp.propDead > 0.5 &&
      sp.leafThickness < 0.00035

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
    val propSilicaFreeAsh: Option[Double],
    val ignitionTemperatureProvided: Option[Double],
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
     * Modelled ignition temperature (will be None if silica free ash proportion was not provided)
     */
    val ignitionTemperatureModelled: Option[Double] = propSilicaFreeAsh map { prop =>
      val logPc = math.log(prop * 100)
      354.0 - 13.9 * logPc - 2.91 * logPc * logPc
    }

    /**
     * Ignition temperature: either the one provided for this species or
     * one modelled from the proportion of silica free ash.
     */
    val ignitionTemperature: Double =
      (ignitionTemperatureProvided orElse ignitionTemperatureModelled).get

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

