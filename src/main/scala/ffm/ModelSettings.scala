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

  /*
   * The increase above ambient temperature for flames in grasses.
   * 
   * See also [[ModelSettings.mainFlameDeltaTemperature]]
   */
  val GrassFlameDeltaTemperature: Double = 750

  /**
   * Proportional reduction in ignition delay time for grasses relative to
   * non-grasses.
   */
  val GrassIDTReduction: Double = 0.75

  /*
   * The increase above ambient temperature for flames in species other than grasses.
   * 
   * See also [[ModelSettings.grassFlameDeltaTemperature]]
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
   * Minimum separation from slope when computing flame angles
   *
   * When computing flame angles the flame cannot adhere to the slope more
   * closely than this setting will allow.
   *
   * Units are radians.
   */
  val MinFlameSepFromSlope: Double = 0.01745

  /**
   * Minimum fuel load for a surface backing fire.
   * 
   * Units are kg / m^2
   */
  val MinFuelLoadForSurfaceBackingFire: Double = 0.4

  /**
   * Minimum fuel load for a surface head fire.
   * 
   * Units are kg / m^2
   */  
  val MinFuelLoadForSurfaceHeadFire = 0.3
  
  /**
   * The minimum height above surface for calculation of wind speed (m).
   */
  val MinHeightForWindModel: Double = 0.1

  /**
   * Minimum temperature for canopy heating.
   *
   * If the canopy is not heated to this value by pre-heating flames then flame
   * residence time is reduced to [[ModelSettings.reducedCanopyFlameResidenceTime]].
   *
   * Units degrees Celsius.
   */
  val MinTempForCanopyHeating: Double = 100

  /**
   * Number of steps used to compute flame penetration at each time step.
   *
   * At each time step the full possible path of ignition (based on flame angles,
   * plume temperatures and ignition temperatures, is divided into numPenetrationSteps
   * and an iterative computation is performed to see how much of this path ignites.
   */
  val NumPenetrationSteps: Int = 10

  /**
   * Reduced canopy flame residence time.
   *
   * If the canopy is not heated to [[ModelSettings.minTempForCanopyHeating]] by pre-heating
   * flames then flame residence time is reduced.
   *
   * Units are seconds.
   */
  val ReducedCanopyFlameResidenceTime: Double = 1

}