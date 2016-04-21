package ffm.util

/**
 * Conversions between units of measurement.
 */
object Units {
  
  class Unit private(val name: String, val abbrev: String) {
    override def toString = s"Unit($name, ($abbrev))"
  }
  
  private object Unit {
    def apply(name: String, abbrev: String): Unit =
      new Unit( munge(name), munge(abbrev) )
  }
  
  val Unitless = Unit("dimensionless", "NA")
  
  val AngleDegree = Unit("degree", "deg")
  val AngleRadian = Unit("radian", "rad")
  val DistanceMetre = Unit("metre", "m")
  val DistanceCentimetre = Unit("centimetre", "cm")
  val DistanceMillimetre = Unit("millimetre", "mm")
  val DistanceKilometre = Unit("kilometre", "km")
  val VelocityKilometresPerHour = Unit("kilometres per hour", "km/h")
  val VelocityMetresPerSecond = Unit("metres per second", "m/s")
  val MassTonne = Unit("tonne", "t")
  val MassKilogram = Unit("kilogram", "kg")
  val ArealMassTonnesPerHectare = Unit("tonnes per hectare", "t/ha")
  val ArealMassKilogramsPerSquareMetre = Unit("kilograms per square metre", "kg/m2")
  val TemperatureCelsius = Unit("degrees Celsius", "degc")
  val TemperatureFarenheit = Unit("degrees Farenheit", "degf")

  // Supported units
  val units: IndexedSeq[Unit] = IndexedSeq(
    Unitless,
    AngleDegree,
    AngleRadian,
    DistanceMetre,
    DistanceCentimetre,
    DistanceMillimetre,
    DistanceKilometre,
    VelocityKilometresPerHour,
    VelocityMetresPerSecond,
    MassTonne,
    MassKilogram,
    ArealMassTonnesPerHectare,
    ArealMassKilogramsPerSquareMetre,
    TemperatureCelsius,
    TemperatureFarenheit)
  
    
  // Provides case and white-space insensitive lookup of unit abbreviations
  private object lookup {
    val m: Map[String, Unit] = Map() ++ (units map { u => (u.abbrev, u) })
    
    def apply(abbrev: String): Option[Unit] =
      m.get( munge(abbrev) )
  }
  
  /** 
   *  Gets the Unit with the given abbreviation.
   *  
   *  The search for the abbreviation is insensitive to case and any leading and/or 
   *  trailing white-space.
   */
  def getUnit(abbrev: String): Unit = lookup(abbrev) match {
    case Some(u) => u
    case None => throw new IllegalArgumentException(s"$abbrev is not a recognized unit abbreviation")
  }
      
  class Converter(
    val fromUnit: Unit,
    val toUnit: Unit,
    val fn: Double => Double
  )
  
  val converters = List(
      new Converter(AngleDegree, AngleRadian, scala.math.toRadians(_)),
      new Converter(AngleRadian, AngleDegree, scala.math.toDegrees(_)),
      new Converter(DistanceMillimetre, DistanceMetre, _ / 1000),
      new Converter(DistanceMetre, DistanceMillimetre, _ * 1000),
      new Converter(DistanceCentimetre, DistanceMetre, _ / 100),
      new Converter(DistanceMetre, DistanceCentimetre, _ * 100),
      new Converter(DistanceKilometre, DistanceMetre, _ * 1000),
      new Converter(DistanceMetre, DistanceKilometre, _ / 1000),
      new Converter(VelocityKilometresPerHour, VelocityMetresPerSecond, _ * 5 / 18),
      new Converter(VelocityMetresPerSecond, VelocityKilometresPerHour, _ * 18 / 5),
      new Converter(MassKilogram, MassTonne, _ / 1000),
      new Converter(MassTonne, MassKilogram, _ * 1000),
      new Converter(ArealMassTonnesPerHectare, ArealMassKilogramsPerSquareMetre, _ / 10),
      new Converter(ArealMassKilogramsPerSquareMetre, ArealMassTonnesPerHectare, _ * 10),
      new Converter(TemperatureCelsius, TemperatureFarenheit, c => c * 9 / 5 + 32),
      new Converter(TemperatureFarenheit, TemperatureCelsius, f => (f - 32) * 5 / 9)
  )
  
  private val converterLookup: Map[ Unit, List[Converter] ] = converters.groupBy { _.fromUnit }
  
  /**
   * Converts a value from one unit to another.
   */
  def convert(fromUnits: Unit, fromValue: Double, toUnits: Unit): Double = {
    if (fromUnits == toUnits) fromValue
    else {
      converterLookup.get(fromUnits) match {
        case Some(convs) =>
          convs.find(_.toUnit == toUnits) match {
            case Some(c) => c.fn(fromValue)
            case None =>
              throw new IllegalArgumentException(s"Conversion from ${fromUnits.name} to ${toUnits.name} not supported")
          }

        case None =>
          throw new IllegalArgumentException(s"Conversion from ${fromUnits.name} to ${toUnits.name} not supported")
      }
    }
  }
  
  /**
   * Converts a value from one unit to another.
   */
  def convert(fromAbbrev: String, fromValue: Double, toAbbrev: String): Double =
    convert( getUnit(fromAbbrev), fromValue, getUnit(toAbbrev) )
    
  /**
   * Converts a value from one unit to another.
   */
  def convert(fromAbbrev: String, fromValue: Double, toUnit: Unit): Double =
    convert( getUnit(fromAbbrev), fromValue, toUnit)
    
  /**
   * Converts a value from one unit to another.
   */
  def convert(fromUnit: Unit, fromValue: Double, toAbbrev: String): Double =
    convert( fromUnit, fromValue, getUnit(toAbbrev) )

  // standardize a string
  private def munge(s: String): String = s.toLowerCase.trim
}
