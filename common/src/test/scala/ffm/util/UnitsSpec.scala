package ffm.util

import ffm.BasicSpec
import org.scalatest.prop.TableDrivenPropertyChecks._

class UnitsSpec extends BasicSpec {
  
  import Units._
  
  val Tol = 1.0e-8
  
  "Conversion based on abbrevations" should "return the correct value" in {
    val tbl = 
      Table(
        ("fromAbbrev", "toAbbrev", "expectedValue"),
        ("deg",        "rad",      scala.math.toRadians(1.0)),
        ("rad",        "deg",      scala.math.toDegrees(1.0)),
        ("m",          "cm",       100.0),
        ("cm",         "m",        0.01),
        ("m",          "mm",       1000.0),
        ("mm",         "m",        0.001),
        ("m",          "km",       0.001),
        ("km",         "m",        1000.0),
        ("km/h",       "m/s",      1.0 / 3.6),
        ("m/s",        "km/h",     3.6),
        ("kg",         "t",        0.001),
        ("t",          "kg",       1000.0),
        ("t/ha",       "kg/m2",    0.1),
        ("kg/m2",      "t/ha",     10.0)
      )
      
    forAll (tbl) { (fromAbbrev: String, toAbbrev: String, expected: Double) =>
      convert(fromAbbrev, 1.0, toAbbrev) should be (expected +- Tol)
    }
  }
  
  "Conversion based on units" should "return the correct value" in {
    val tbl = 
      Table(
        ("fromUnit",                       "toUnit",                          "expectedValue"),
        (AngleDegree,                      AngleRadian,                       math.Pi / 180),
        (AngleRadian,                      AngleDegree,                       180 / math.Pi),
        (DistanceMetre,                    DistanceCentimetre,                100.0),
        (DistanceCentimetre,               DistanceMetre,                     0.01),
        (DistanceMetre,                    DistanceMillimetre,                1000.0),
        (DistanceMillimetre,               DistanceMetre,                     0.001),
        (DistanceMetre,                    DistanceKilometre,                 0.001),
        (DistanceKilometre,                DistanceMetre,                     1000.0),
        (VelocityKilometresPerHour,        VelocityMetresPerSecond,           1.0 / 3.6),
        (VelocityMetresPerSecond,          VelocityKilometresPerHour,         3.6),
        (MassKilogram,                     MassTonne,                         0.001),
        (MassTonne,                        MassKilogram,                      1000.0),
        (ArealMassTonnesPerHectare,        ArealMassKilogramsPerSquareMetre,  0.1),
        (ArealMassKilogramsPerSquareMetre, ArealMassTonnesPerHectare,         10.0)
      )
      
    forAll (tbl) { (fromUnit: Unit, toUnit: Unit, expected: Double) =>
      convert(fromUnit, 1.0, toUnit) should be (expected +- Tol)
    }
  }
  
  "Conversion based on unit to abbreviation" should "return the correct value" in {
    val tbl = 
      Table(
        ("fromUnit",                       "toAbbrev", "expectedValue"),
        (AngleDegree,                      "rad",      math.Pi / 180),
        (AngleRadian,                      "deg",      180 / math.Pi),
        (DistanceMetre,                    "cm",       100.0),
        (DistanceCentimetre,               "m",        0.01),
        (DistanceMetre,                    "mm",       1000.0),
        (DistanceMillimetre,               "m",        0.001),
        (DistanceMetre,                    "km",       0.001),
        (DistanceKilometre,                "m",        1000.0),
        (VelocityKilometresPerHour,        "m/s",      1.0 / 3.6),
        (VelocityMetresPerSecond,          "km/h",     3.6),
        (MassKilogram,                     "t",        0.001),
        (MassTonne,                        "kg",       1000.0),
        (ArealMassTonnesPerHectare,        "kg/m2",    0.1),
        (ArealMassKilogramsPerSquareMetre, "t/ha",     10.0)
      )
      
    forAll (tbl) { (fromUnit: Unit, toAbbrev: String, expected: Double) =>
      convert(fromUnit, 1.0, toAbbrev) should be (expected +- Tol)
    }
  }
  
  "Conversion based on abbreviation to unit" should "return the correct value" in {
    val tbl = 
      Table(
        ("fromAbbrev", "toUnit",                         "expectedValue"),
        ("rad",        AngleDegree,                      180 / math.Pi),
        ("deg",        AngleRadian,                      math.Pi / 180),
        ("cm",         DistanceMetre,                    0.01),
        ("m",          DistanceCentimetre,               100.0),
        ("mm",         DistanceMetre,                    0.001),
        ("m",          DistanceMillimetre,               1000.0),
        ("km",         DistanceMetre,                    1000.0),
        ("m",          DistanceKilometre,                0.001),
        ("m/s",        VelocityKilometresPerHour,        3.6),
        ("km/h",       VelocityMetresPerSecond,          1.0 / 3.6),
        ("t",          MassKilogram,                     1000.0),
        ("kg",         MassTonne,                        0.001),
        ("kg/m2",      ArealMassTonnesPerHectare,        10.0),
        ("t/ha",       ArealMassKilogramsPerSquareMetre, 0.1)
      )
      
    forAll (tbl) { (fromAbbrev: String, toUnit: Unit, expected: Double) =>
      convert(fromAbbrev, 1.0, toUnit) should be (expected +- Tol)
    }
  }
  
  "Conversion from an unrecognized abbreviation" should "throw an exception" in {
    intercept[IllegalArgumentException] {
      convert("potato", 1.0, AngleDegree)
    }
  }

  "Conversion to an unrecognized abbreviation" should "throw an exception" in {
    intercept[IllegalArgumentException] {
      convert(AngleDegree, 1.0, "potato")
    }
  }
  
  "Conversion of a unit to itself" should "return the input value" in {
    val tbl = Table(("unit"), Units.units: _*)
    
    forAll (tbl) { u =>    
      val x = scala.util.Random.nextDouble()
      convert(u, x, u) should be (x)
    }
  }
  
  "Conversion of an abbreviation to itself" should "return the input value" in {
    val tbl = Table(("abbrev"), (Units.units.map(_.abbrev)): _*)
    
    forAll (tbl) { abbrev =>    
      val x = scala.util.Random.nextDouble()
      convert(abbrev, x, abbrev) should be (x)
    }
  }
    
  "Conversion of a unit to its own abbreviation or vice versa" should "return the input value" in {
    val tbl = Table(("unit", "abbrev"), (Units.units.map(u => (u, u.abbrev))): _*)
    
    forAll (tbl) { (u, abbrev) =>    
      val x = scala.util.Random.nextDouble()
      convert(u, x, abbrev) should be (x)
      convert(abbrev, x, u) should be (x)
    }
  }
    
  "Abbreviations" should "be matched ignoring case and white-space" in {
    val tbl = Table(("unit", "alteredAbbrev"), (Units.units.map(u => (u, s" ${u.abbrev.toUpperCase} ")): _*))
    
    forAll (tbl) { (u, alteredAbbrev) =>
      getUnit(alteredAbbrev) should be (u)
    }
  }
  
  "Converting a unitless value to itself" should "return the input value" in {
    convert(Unitless, 42.0, Unitless) should be (42.0)
  }
  
  "Converting a unitless value to any unit or vice versa" should "throw an exception" in {
    val tbl = Table(("unit"), Units.units.filterNot(_ == Unitless): _*)
    
    forAll (tbl) { u =>    
      intercept[IllegalArgumentException] {
        convert(Unitless, 1.0, u)
      }
      
      intercept[IllegalArgumentException] {
        convert(u, 1.0, Unitless)
      }
    }
    
  }
}
