package ffm.fire

import ffm.geometry._
import ffm.numerics.Numerics
import ffm.forest.Stratum


class Flame(flen: Double, val angle: Double, val origin: Coord, val depthIgnited: Double, val deltaTemperature: Double) {

  val flameLength = flen.max(depthIgnited)

  val isNull = Numerics.leq(flameLength, 0.0)

  val tip = origin.bearing(angle, flameLength)

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
   * Returns None if this flame is not burning (ie. isNull is true).
   *
   * This is from Section 5.21 of Zylstra's thesis, with corrections.
   * See also Weber et al as referenced in thesis.
   */
  def plumeDeltaTemperature(dist: Double): Option[Double] = {
    // Helper method
    def calculatedTemp = {
      val a = -1.0 / (flameLength * (flameLength - depthIgnited))
      if (dist <= flameLength)
        deltaTemperature * math.exp(a * (dist - depthIgnited) * (dist - depthIgnited))
      else
        deltaTemperature * flameLength / dist * math.exp(a * (flameLength - depthIgnited) * (flameLength - depthIgnited))
    }
    
    if (isNull) None 
    else if (dist <= depthIgnited) Some(deltaTemperature) 
    else Some(calculatedTemp)
  }

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame with the given ambient temperature.
   * 
   * Returns None if this flame is not burning (ie. isNull is true).
   */
  def plumeTemperature(dist: Double, ambientTemp: Double): Option[Double] =
    plumeDeltaTemperature(dist) map (_ + ambientTemp)

  /**
   * Determines the temperature at some point with the given ambient temperature.
   * Returns None if this flame is not burning (ie. isNull is true).
   */
  def plumeTemperature(pos: Coord, ambientTemp: Double): Option[Double] =
    plumeTemperature(origin.distanceTo(pos), ambientTemp)

  // inversion of deltaPlumeTemp, returns distance from origin at which targetTemp is achieved
  def inversePlumeTemperature(targetTemp: Double, ambientTemp: Double): Option[Double] = {
    if (flameLength < 0 || depthIgnited < 0 || flameLength < depthIgnited)
      None
    else if (Numerics.gt(targetTemp, deltaTemperature + ambientTemp))
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

case class PreHeatingFlame(flame: Flame, level: Stratum.Level, startTime: Double, endTime: Double)

object Flame {
  def apply(flameLength: Double, angle: Double, origin: Coord, depthIgnited: Double, deltaTemperature: Double) = 
    new Flame(flameLength, angle, origin, depthIgnited, deltaTemperature)
}
