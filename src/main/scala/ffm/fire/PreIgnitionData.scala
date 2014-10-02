package ffm.fire

abstract class PreIgnitionData(
  val timeStep: Int,
  val flame: Flame,
  val distanceToFlame: Double,
  val dryingFactor: Double,
  val dryingTemperature: Double) extends AnyRef {
  
  def flameLength = flame.flameLength 
  def flameAngle = flame.angle
  def depth = flame.depthIgnited
} 

/**
 * Data on drying due to a pre-heating flame prior to ignition.
 */
class PreHeatingDrying(
  timeStep: Int,
  preHeatingFlame: PreHeatingFlame,
  distanceToFlame: Double,
  dryingFactor: Double,
  dryingTemperature: Double,
  val duration: Double) extends PreIgnitionData(timeStep, preHeatingFlame.flame, distanceToFlame, dryingFactor, dryingTemperature)

/**
 * Data on drying due to an incident flame prior to ignition.
 */
class IncidentDrying(
  timeStep: Int,
  flame: Flame,
  distanceToFlame: Double,
  dryingFactor: Double,
  dryingTemperature: Double,
  val ignitionDelayTime: Double) extends PreIgnitionData(timeStep, flame, distanceToFlame, dryingFactor, dryingTemperature)

