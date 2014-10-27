package ffm.fire

import scala.collection.mutable.ArrayBuffer

import ffm.ModelSettings
import ffm.geometry._
import ffm.numerics.Numerics
import ffm.numerics.Numerics._


class Flame(val flameLength: Double, val angle: Double, val origin: Coord, val depthIgnited: Double, val deltaTemperature: Double) {

  // Unlike the original C++ code we don't allow null flames
  require( flameLength > 0, "flame length must be greater than 0")
  require( depthIgnited >= 0, "depth Ignited must be 0 or greater")
  require( flameLength > depthIgnited, "flame length must be greater than depth ignited")

  val tip = origin.toBearing(angle, flameLength)

  val plume = Ray(origin, angle)
  
  /** Creates a copy of this flame. */
  def copy = new Flame(flameLength, angle, origin, depthIgnited, deltaTemperature)
  
  /**
   * Returns a flame with the same attributes as this one but at a
   * new origin coordinate.
   */
  def toOrigin(newOrigin: Coord) = 
    new Flame(flameLength, angle, newOrigin, depthIgnited, deltaTemperature)

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame. 
   * 
   * This is from Section 5.21 of Zylstra's thesis, with corrections.
   * See also Weber et al as referenced in thesis.
   */
  def plumeDeltaTemperature(dist: Double): Double = {
    def calculatedTemp = {
      val dlen = flameLength - depthIgnited
      val a = -1.0 / (2.0 * flameLength * dlen)
      
      if (dist <= flameLength)
        deltaTemperature * math.exp(a * (dist - depthIgnited) * (dist - depthIgnited))
      else
        deltaTemperature * flameLength / dist * math.exp(a * dlen * dlen)
    }
    
    if (dist <= depthIgnited) deltaTemperature
    else calculatedTemp
  }

  /**
   * Determines the temperature at some distance along the plume from the origin of 
   * this flame with the given ambient temperature.
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
    else
      Some(
        if (targetTemp.almostEq(deltaTemperature + ambientTemp))
          depthIgnited
        else {
          val deltaT = targetTemp - ambientTemp
          val dlen = flameLength - depthIgnited
          val a = -1.0 / (2 * flameLength * dlen)

          if (Numerics.gt(deltaT, deltaTemperature * math.exp(a * dlen * dlen)))
            math.sqrt(math.log(deltaT / deltaTemperature) / a) + depthIgnited
          else
            deltaTemperature * flameLength / deltaT * math.exp(a * dlen * dlen)
        }
      )
  }

}


/**
 * Companion object to the Flame class
 */
object Flame {
  /**
   * Creates a new flame.
   */
  def apply(length: Double, angle: Double, origin: Coord, depthIgnited: Double, deltaTemperature: Double) = 
    new Flame(length, angle, origin, depthIgnited, deltaTemperature)

  /**
   * Calculates flame angle.
   */
  def flameAngle(flameLength: Double, windSpeed: Double, slope: Double, fireLineLength: Double): Double = {
    if (Numerics.almostZero(flameLength))
      0.0   
    else if (Numerics.almostZero(slope))
      windEffectFlameAngle(flameLength, windSpeed, slope)
    else {
      if (slope > 0) {
        if (windSpeed >= 0) {
          math.min(
            windEffectFlameAngle(flameLength, windSpeed, slope),
            slopeEffectFlameAngle(flameLength, slope, fireLineLength))
            
        } else {  // windSpeed < 0
          if (math.abs(windSpeed) <= ModelSettings.SlopeDominanceWindThreshold)
            slopeEffectFlameAngle(flameLength, slope, fireLineLength)
          else
            windEffectFlameAngle(flameLength, windSpeed, slope)
        }
      } else {  // slope < 0
        if (windSpeed <= 0) {
          math.max(
            windEffectFlameAngle(flameLength, windSpeed, slope),
            slopeEffectFlameAngle(flameLength, slope, fireLineLength))
          
        } else {  // windSpeed < 0
          if (math.abs(windSpeed) <= ModelSettings.SlopeDominanceWindThreshold)
            slopeEffectFlameAngle(flameLength, slope, fireLineLength)
          else
            windEffectFlameAngle(flameLength, windSpeed, slope)
        }
      }
    }
  }

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

  /**
   *  Calculates flame angle given slope and fire line length (no wind effect).
   */
  def slopeEffectFlameAngle(flameLength: Double, slope: Double, fireLineLength: Double): Double = {
    if (Numerics.almostZero(flameLength)) 0.0
    else {
      val effSlope = if (flameLength < fireLineLength) slope else effectiveSlope(slope)

      val term =
        if (effSlope >= 0)
          math.max(math.Pi - effSlope, 0.25 * math.Pi)
        else
          math.min(math.Pi - effSlope, 0.75 * math.Pi)

      math.min(
        math.Pi + slope - ModelSettings.MinFlameSepFromSlope,
        math.max(term, slope + ModelSettings.MinFlameSepFromSlope))
    }
  }

  /**
   * Calculates effective slope as per Zylstra's thesis section 5.222.
   *
   * Notes from C++ version of the model:
   * Want to find the mean of arcsin(sin(theta_g)*cos(theta)) for theta in [-PI/2,PI/2]
   * Use Newton-Cotes quadrature of arcsin(sin(theta_g)*cos(theta)) for theta in [0,PI/2]
   * because of symmetry. Increase n for more accurate result (20 seems adequate).
   */
  private def effectiveSlope(slope: Double): Double = {
    val n = 20
    val h = 0.5 * math.Pi / n
    val s = math.sin(slope)

    val sum = ( (1 until n) map (k => math.asin(s * math.cos(k * h))) ).sum

    (0.5 * slope + sum) / n
  }
  
  /**
   * Combines two sets of flames with the angles of the resulting flames
   * calculated using wind speed, slope and fire line length.
   */
  def combineFlames(flames1: IndexedSeq[Flame], flames2: IndexedSeq[Flame],
                    windSpeed: Double, slope: Double, fireLineLength: Double): IndexedSeq[Flame] = {
    
    val combiner = combineFlames(_ :Flame, _ :Flame, windSpeed, slope, fireLineLength)
    combineFlames(flames1, flames2, combiner)
  }
  
  /**
   * Combines two sets of flames with the angles of the resulting flames
   * calculated as length-weighted averages.
   */
  def combineFlames(flames1: IndexedSeq[Flame], flames2: IndexedSeq[Flame]): IndexedSeq[Flame] = {
    
    val combiner = combineFlames(_ :Flame, _ :Flame)
    combineFlames(flames1, flames2, combiner)
  }
  
  /**
   * Private method to combine two sets of flames using the given combiner function.
   */
  private def combineFlames(flames1: IndexedSeq[Flame], flames2: IndexedSeq[Flame], combiner: (Flame, Flame) => Flame): IndexedSeq[Flame] = {
    if (flames1.isEmpty) flames2
    else if (flames2.isEmpty) flames1
    else {
      val combined = for {
        (f1, f2) <- flames1 zip flames2
      } yield combiner(f1, f2)
      
      // Return combined flames plus any left-over
      val n1 = flames1.length
      val n2 = flames2.length
      
      if (n1 > n2)
        combined ++ flames1.drop( n2 )
      else if (n2 > n1)
        combined ++ flames2.drop( n1 )
      else 
        combined
    }
  }
  
  /**
   * Combines two flames with flame angle calculated from wind speed, slope and fire line length.
   */  
  def combineFlames(flame1: Flame, flame2: Flame,
                    windSpeed: Double, slope: Double, fireLineLength: Double): Flame = {
    
    val origin = lowestOrigin(flame1, flame2)
    val depthIgnited = flame1.depthIgnited + flame2.depthIgnited        
    val temperature = weightedAverageTemperature(flame1, flame2)
    val combinedLength = combinedFlameLength(flame1, flame2)    
    val angle = flameAngle(combinedLength, windSpeed, slope, fireLineLength)
    
    Flame(combinedLength, angle, origin, depthIgnited, temperature)
  }

  /**
   * Combines two flames with flame angle calculated as a length-weighted average of the input angles.
   */
  def combineFlames(flame1: Flame, flame2: Flame): Flame = {
    val origin = lowestOrigin(flame1, flame2)
    val depthIgnited = flame1.depthIgnited + flame2.depthIgnited
    val temperature = weightedAverageTemperature(flame1, flame2)
    val combinedLength = combinedFlameLength(flame1, flame2)
    val angle = (flame1.flameLength * flame1.angle + flame2.flameLength * flame2.angle) /
    (flame1.flameLength + flame2.flameLength)
    
    Flame(combinedLength, angle, origin, depthIgnited, temperature)
  }
    
  /**
   * Lowest origin of two flames.
   */
  def lowestOrigin(flame1: Flame, flame2: Flame): Coord =
    if (flame1.origin.y <= flame2.origin.y) flame1.origin else flame2.origin
    
  /**
   * Average temperature of two flames weighted by flame length.
   */
  def weightedAverageTemperature(flame1: Flame, flame2: Flame): Double =
    (flame1.deltaTemperature * flame1.flameLength + flame2.deltaTemperature * flame2.flameLength) /
    (flame1.flameLength + flame2.flameLength)

  /**
   * Calculates combined flame length based on vertical overlaps.
   */
  def combinedFlameLength(flame1: Flame, flame2: Flame): Double = {
    val verticalOverlap = 
      math.max(0.0,
               math.min(flame1.tip.y, flame2.tip.y) - math.max(flame1.origin.y, flame2.origin.y))
     
    val overlap1 = verticalOverlap / (flame1.tip.y - flame1.origin.y) * flame1.flameLength 
    val overlap2 = verticalOverlap / (flame2.tip.y - flame2.origin.y) * flame2.flameLength
    
    val initLen = flame1.flameLength + flame2.flameLength - (overlap1 + overlap2) / 2
    
    math.max(initLen, math.max(flame1.flameLength, math.max(flame2.flameLength, flame1.depthIgnited)))
  }

  /**
   * Calculates the length of a laterally merged flame in a stratum based on average 
   * crown width and separation.
   */
  def lateralMergedFlameLength(flameLength: Double, fireLineLength: Double, crownWidth: Double, crownSeparation: Double): Double = {
    val sigma = fireLineLength min (0.23112 * crownWidth * math.pow(flameLength / crownWidth, 2.0 / 3.0))
    flameLength * math.pow(sigma / crownSeparation + 1.0, 0.4)
  }

}
