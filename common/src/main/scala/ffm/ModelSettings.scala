package ffm

object ModelSettings {

  /**
   * The atomic increment in time for computations.
   */
  val ComputationTimeInterval: Double = 1.0

  /**
   * Maximum dead fuel moisture content (proportion) for a surface fire to exist.
   */
  val ExtinctionDFMC: Double = 0.2

  /**
   * The increase above ambient temperature for flames in grasses.
   * 
   * See also [[MainFlameDeltaTemperature]]
   */
  val GrassFlameDeltaTemperature: Double = 750

  /**
   * Proportional reduction in ignition delay time for grasses relative to
   * non-grasses.
   */
  val GrassIDTReduction: Double = 0.75

  /**
   * The increase above ambient temperature for flames in species other than grasses.
   * 
   * See also [[GrassFlameDeltaTemperature]]
   */
  val MainFlameDeltaTemperature: Double = 950
  
  /**
   * Maximum slope that will be used to compute a surface head fire rate of spread.
   * 
   * The value is 40 degrees (0.6981 radians).
   */
  val MaxSlopeForSurfaceROS = 0.6981
  
  /**
   * Maximum surface fire flame length (m).
   */
  val MaxSurfaceFlameLength: Double = 2.0
  
  /**
   * Maximum rate of spread for any surface fire (m/s).
   */
  val MaxSurfaceROS: Double = 0.0972

  /** 
   * The maximum length of time for any single computation of an IgnitionPath. 
   */
  val MaxIgnitionTime: Double = 25.0

  /**
   *  The maximum number of time steps that will be used for the computation
   *  of any single IgnitionPath.
   */
  val MaxIgnitionTimeSteps: Int = (MaxIgnitionTime / ComputationTimeInterval + 0.5).toInt

  /**
   * Minimum separation from slope when computing flame angles (radians).
   *
   * When computing flame angles the flame cannot adhere to the slope more
   * closely than this setting will allow.
   */
  val MinFlameSepFromSlope: Double = 0.01745

  /**
   * Minimum fuel load for a surface backing fire (kg / m^2).
   */
  val MinFuelLoadForSurfaceBackingFire: Double = 0.4

  /**
   * Minimum fuel load for a surface head fire (kg / m^2).
   */  
  val MinFuelLoadForSurfaceHeadFire = 0.3
  
  /**
   * The minimum height above surface for calculation of wind speed (m).
   */
  val MinHeightForWindModel: Double = 0.1
  
  /**
   * The minimum rate of spread for a stratum fire to be classified as 
   * spreading (m/s).
   *
   * A stratum fire must spread at this rate or faster for 
   * [[MinTimeStepsForStratumSpread]] time steps to be classified as 
   * spreading through the stratum.
   */
  val MinRateForStratumSpread: Double = 2.7778e-3  // == 0.01 km/h

  /**
   * Minimum temperature for canopy heating (degrees Celsius).
   *
   * If the canopy is not heated to this value by pre-heating flames then flame
   * residence time is reduced to [[ReducedCanopyFlameResidenceTime]].
   */
  val MinTempForCanopyHeating: Double = 100

  /**
   * The minimum number of time steps that a stratum fire must spread to be
   * classified as spreading through the stratum.
   *
   * A stratum fire must spread at at least this rate for 
   * [[MinTimeStepsForStratumSpread]] time steps to be classified as 
   * spreading through the stratum.
   */
  val MinTimeStepsForStratumSpread = 2
  
  /**
   * Number of steps used to compute flame penetration at each time step.
   *
   * At each time step the full possible path of ignition (based on flame angles,
   * plume temperatures and ignition temperatures, is divided into numPenetrationSteps
   * and an iterative computation is performed to see how much of this path ignites.
   */
  val NumPenetrationSteps: Int = 10

  /**
   * Reduced canopy flame residence time (s).
   *
   * If the canopy is not heated to [[MinTempForCanopyHeating]] by pre-heating
   * flames then flame residence time is reduced.
   */
  val ReducedCanopyFlameResidenceTime: Double = 1
  
  /**
   * Threshold wind speed for flame angles to be computed using wind effect (m/s).
   * 
   * If the absolute value of the wind speed is less than this threshold
   * flame angles are computed using the slope effect model, otherwise they are 
   * computed using the wind effect model
   */
  val SlopeDominanceWindThreshold: Double = 0.8333
  
  /**
   * The width to use for the artificial crown polygon in stratum flame runs.
   */
  val StratumBigCrownWidth: Double = 10000.0
  
  
  /////////////////////////////////////////////////////////////////////////////
  // EXPERIMENTAL SETTINGS
  /////////////////////////////////////////////////////////////////////////////
  
  /**
   * Number of decimal places to use for precision-sensitive calculations
   * involving distances and positions.
   * 
   * Example: this is used to limit the precision of species crown polygon
   * vertices and intersection with flame plumes. Distances are represented
   * in metres, so DistancePrecision of 4 equates to 0.1 mm precision.
   */
  val DistancePrecision: Integer = 4
}