package ffm.fire

import ffm.ModelSettings
import ffm.forest.Surface
import ffm.numerics._

class DefaultSurfaceFireAttributes(sf: Surface) extends SurfaceFireAttributes {

  /**
   * Thickness multiplier.
   *
   * Taken from McArthur 1966 and rendered to equation form in Zylstra 2013
   * thicknessMultiplier = 0.5314*meanFineLeaves_^-0.401
   * where meanFineLeaves_ is in mm.
   */
  val thicknessMultiplier: Double = 0.5314 * math.pow(sf.meanFinenessLeaves * 1000, -0.401)

  /**
   * Rate of spread for backing surface fire (m/s).
   */
  val backingROS: Double =
    if (sf.deadFuelMoistureProp >= ModelSettings.ExtinctionDFMC) 0.0
    else if (sf.fuelLoad < ModelSettings.MinFuelLoadForSurfaceBackingFire) 0.0
    else thicknessMultiplier * (2.703e-3 * sf.fuelLoad + 1.175e-3) * math.exp(3.9534 * sf.slope)

  /**
   * Flame length for a backing surface fire (m).
   */
  val backingFlameLength: Double =
    if (sf.deadFuelMoistureProp >= ModelSettings.ExtinctionDFMC) 0.0
    else if (sf.fuelLoad < ModelSettings.MinFuelLoadForSurfaceBackingFire) 0.0
    else 0.317 * sf.fuelLoad + 0.0167

  /**
   * Rate of spread for heading surface fire (m/s).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def headROS(surfaceWindSpeed: Double): Double =
    if (sf.deadFuelMoistureProp >= ModelSettings.ExtinctionDFMC) 0.0
    else if (sf.fuelLoad < ModelSettings.MinFuelLoadForSurfaceHeadFire) 0.0
    else {
      val s = math.max(0.0, math.min(sf.slope, ModelSettings.MaxSlopeForSurfaceROS))
      val term1 = 0.42088 * math.pow(surfaceWindSpeed, 2.22) + 0.071
      val term2 = (10.8 + 3.3192 * sf.deadFuelMoistureProp)
      thicknessMultiplier * (term1 / term2) * math.exp(3.9534 * s)
    }

  /**
   * Flame length for heading surface file (m).
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def headFlameLength(surfaceWindSpeed: Double): Double =
    if (sf.deadFuelMoistureProp >= ModelSettings.ExtinctionDFMC) 0.0
    else if (sf.fuelLoad < ModelSettings.MinFuelLoadForSurfaceHeadFire) 0.0
    else 8.64 * headROS(surfaceWindSpeed) + 0.036 * sf.fuelLoad

  /**
   * Overall surface fire rate of spread (m/s).
   *
   * Applies to both heading and backing fires.
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def ros(surfaceWindSpeed: Double): Double =
    math.min(ModelSettings.MaxSurfaceROS,
      math.max(backingROS, headROS(surfaceWindSpeed)))

  /**
   * Overall surface fire flame length (m).
   *
   * Applies to both heading and backing fires.
   *
   * @param surfaceWindSpeed surface wind speed (m/s)
   */
  def flameLength(surfaceWindSpeed: Double): Double = {
    val len = if (surfaceWindSpeed > 0) headFlameLength(surfaceWindSpeed) else backingFlameLength
    math.min(len, ModelSettings.MaxSurfaceFlameLength)
  }

  /**
   * Surface fire flame residence time (s).
   */
  val flameResidenceTime: Double =
    0.87 * math.pow(sf.meanFuelDiameter * 1000, 1.875)

}
