package ffm.fire

import ffm.ModelSettings
import ffm.geometry._
import ffm.numerics.Numerics


class Flame(flen: Double, val angle: Double, val origin: Coord, val depthIgnited: Double, val deltaTemperature: Double) {

  val flameLength = flen.max(depthIgnited)  
  
  // Unlike the original C++ code we don't allow null flames
  require( flameLength > 0, "flame length must be greater than 0")

  val tip = origin.toBearing(angle, flameLength)

  val plume = Ray(origin, angle)
  
  /**
   * Returns a flame with the same properties as this one but at a
   * new origin coordinate.
   */
  def toOrigin(newOrigin: Coord) = 
    Flame(flameLength, angle, newOrigin, depthIgnited, deltaTemperature)

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame. 
   * 
   * This is from Section 5.21 of Zylstra's thesis, with corrections.
   * See also Weber et al as referenced in thesis.
   */
  def plumeDeltaTemperature(dist: Double): Double = {
    // Helper method
    def calculatedTemp = {
      val a = -1.0 / (flameLength * (flameLength - depthIgnited))
      if (dist <= flameLength)
        deltaTemperature * math.exp(a * (dist - depthIgnited) * (dist - depthIgnited))
      else
        deltaTemperature * flameLength / dist * math.exp(a * (flameLength - depthIgnited) * (flameLength - depthIgnited))
    }
    
    if (dist <= depthIgnited) deltaTemperature
    else calculatedTemp
  }

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame with the given ambient temperature.
   * 
   * Returns None if this flame is not burning (ie. isNull is true).
   */
  def plumeTemperature(dist: Double, ambientTemp: Double): Double =
    plumeDeltaTemperature(dist) + ambientTemp

  /** 
   * Determines the distance from origin at which targetTemp is achieved.
   * This is the inverse of plumeDeltaTemperature.
   * 
   * Returns None if the target temperature is higher than the flame temperature.
   */
  def distanceForTemperature(targetTemp: Double, ambientTemp: Double): Option[Double] = {
    if (Numerics.gt(targetTemp, deltaTemperature + ambientTemp))
      None
    else {
      val rtnVal =
        if (Numerics.almostEq(targetTemp, deltaTemperature + ambientTemp))
          depthIgnited
        else {
          val deltaT = targetTemp - ambientTemp
          val a = -1.0 / (2 * flameLength * (flameLength - depthIgnited))
          val deltaLen = flameLength - depthIgnited

          if (Numerics.gt(deltaT, deltaTemperature * math.exp(a * deltaLen * deltaLen)))
            math.sqrt(math.log(deltaT / deltaTemperature) / a) + depthIgnited
          else
            deltaTemperature * flameLength / deltaT * math.exp(a * deltaLen * deltaLen)
        }
      Some(rtnVal)
    }
  }

}


object Flame {
  def apply(flameLength: Double, angle: Double, origin: Coord, depthIgnited: Double, deltaTemperature: Double) = 
    new Flame(flameLength, angle, origin, depthIgnited, deltaTemperature)

  /**
   * Calculates flame angle from wind speed and slope.
   */
  def windEffectFlameAngle(flameLength: Double, windSpeed: Double, slope: Double): Double = {
    if (Numerics.almostZero(flameLength)) 0.0
    else if (Numerics.almostZero(windSpeed)) math.Pi / 2
    else {
      val effect =
        if (windSpeed > 0)
          math.atan(0.88664 * math.pow(flameLength, 1.085) / math.pow(windSpeed, 1.5))
        else
          math.Pi - math.atan(0.88664 * math.pow(flameLength, 1.085) / math.pow(windSpeed.abs, 1.5))

      math.min(
        math.Pi + slope - ModelSettings.MinFlameSepFromSlope,
        math.max(effect, slope + ModelSettings.MinFlameSepFromSlope))
    }
  }
}
