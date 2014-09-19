package ffm.geometry

import ffm.numerics.Numerics._

/**
 * Provides methods to work with angular values.
 */
object Angles {
  
  import com.vividsolutions.jts.algorithm.{Angle => JTSAngle}
  
  val Pi = math.Pi
  val TwoPi = 2 * Pi
  val PiOnTwo = Pi / 2
  val PiOnFour = Pi / 4

  /**
   * Calculates the smallest absolute difference between two angles.
   * 
   * Note: This differs from the equivalent method in the JTS library 
   * Angle class by being able to deal with arbitrary angles.
   */
  def diff(theta1: Double, theta2: Double): Double = {
    val delta = math.abs(theta1 - theta2) % TwoPi
    math.min(delta, TwoPi - delta)
  }
  
  /**
   * Returns true if the smallest absolute difference between two angles
   * is almost zero.
   */
  def almostEq(theta1: Double, theta2: Double): Boolean =
    diff(theta1, theta2).almostZero
  
  /**
   * Normalizes an angle to lie in the interval (-Pi, Pi]
   */
  def normalizePi(theta: Double): Double =
    JTSAngle.normalize(theta)
    
  /**
   * Normalizes an angle to lie in the interval [0, 2Pi)
   */
  def normalizeTwoPi(theta: Double): Double =
    JTSAngle.normalizePositive(theta)
}
