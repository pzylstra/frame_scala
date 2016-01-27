package ffm.util

/**
 * Conversions between units of measurement.
 */
object Units {
  
  sealed trait Unit {
    def name: String
    def abbrev: String
  }
  
  private case class UnitInstance(name: String, abbrev: String) extends Unit
  
  val units: List[Unit] = List(
      UnitInstance("degree", "deg"),
      UnitInstance("radian", "rad"),
      UnitInstance("metre", "m"),
      UnitInstance("centimetre", "cm"),
      UnitInstance("millimetre", "mm"),
      UnitInstance("kilometre", "km"),
      UnitInstance("kilometres per hour", "km/h"),
      UnitInstance("metres per second", "m/s"),
      UnitInstance("tonne", "t"),
      UnitInstance("kilogram", "k")
  )
  
  private val lookup: Map[String, Unit] = Map() ++ (units map { u => (u.abbrev, u) })
  
  def getUnit(abbrev: String): Unit = lookup.get(abbrev) match {
    case Some(u) => u
    case None => throw new IllegalArgumentException(s"$abbrev is not a recognized unit abbreviation")
  }
    
  case class Measure(value: Double, unit: Unit) {
  
    /** 
     * Create from unit abbreviation. 
     * 
     * Throws an exception if the abbreviation is not valid. 
     */
    def this(value: Double, abbrev: String) = this(value, getUnit(abbrev))

  }  
  
  class Converter(
    val fromAbbrev: String,
    val toAbbrev: String,
    val fn: Double => Double
  )
  
  val converters = List(
      new Converter("deg", "rad", scala.math.toRadians(_)),
      new Converter("rad", "deg", scala.math.toDegrees(_)),
      new Converter("mm", "m", _ / 1000),
      new Converter("m", "mm", _ * 1000),
      new Converter("cm", "m", _ / 100),
      new Converter("m", "cm", _ * 100),
      new Converter("km/h", "m/s", _ * 5 / 18),
      new Converter("m/s", "km/h", _ * 18 / 5),
      new Converter("kg", "t", _ / 1000),
      new Converter("t", "kg", _ * 1000),
      new Converter("t/ha", "kg/m2", _ / 10)
  )
  
  private val converterLookup: Map[ String, List[Converter] ] = converters.groupBy { _.fromAbbrev }
  
  def convert(fromAbbrev: String, toAbbrev: String, value: Double): Double = {
    converterLookup.get(fromAbbrev) match { 
      case Some(cs) =>
        cs.find { _.toAbbrev == toAbbrev } match {
          case Some(c) => c.fn(value)
          case None =>
            throw new IllegalArgumentException(s"unrecognized unit abbreviation: $toAbbrev")
        }  
        
      case None =>
        throw new IllegalArgumentException(s"unrecognized unit abbreviation: $fromAbbrev")
    }
  }
    

}
