package ffm.io.r

import ffm.BasicSpec
import ffm.util.Units

class ObjectFactorySpec extends BasicSpec {
  
  import ObjectFactory._
  import ParamTables._
  
  import ParamTestData.ParamsNoUnits
  
  val Tol = 1.0e-8
    
  "ParamLookup" should "be case insensitive for simple value searches" in {
    val ps = Array(
        Array("NA", "NA", "slope", "10"),
        Array("NA", "NA", "deadFuelMoistureProp", "0.05")
        )
        
    val lu = new ParamLookup(ps.recs)
    lu("slope") should be ("10")
    lu("deadFuelMoistureProp") should be ("0.05")
  }
  
  it should "be case insensitive for Double value searches" in {
    val ps = Array(
        Array("NA", "NA", "slope", "10"),
        Array("NA", "NA", "deadFuelMoistureProp", "0.05")
        )
        
    val lu = new ParamLookup(ps.recs)
    lu.dval("slope") should be (math.toRadians(10.0) +- Tol)
    lu.dval("deadFuelMoistureProp") should be (0.05)
  }
  
  it should "fail with duplicate parameter labels by default or when allowDuplicates arg is false" in {
    val ps = Array(
        Array("NA", "NA", "overlapping", "nearsurface, midstorey"),
        Array("NA", "NA", "overlapping", "midstorey, canopy")
        )
    
    intercept[IllegalArgumentException]{ 
      new ParamLookup(ps.recs, allowDuplicates = false)
    }
        
    // default (allowDuplicates = false)
    intercept[IllegalArgumentException]{ 
      new ParamLookup(ps.recs)
    }    
  }
  
  it should "allow duplicate parameter labels when that is specified" in {
    val ps = Array(
        Array("NA", "NA", "overlapping", "nearsurface, midstorey"),
        Array("NA", "NA", "overlapping", "midstorey, canopy")
        )
    
    new ParamLookup(ps.recs, allowDuplicates = true)
  }
  
  it should "ignore unrecognized parameter labels" in {
    val ps = Array(
        Array("NA", "NA", "leafLength", "0.01"),        // supported
        Array("NA", "NA", "coffee", "double espresso")  // unsupported
        )
    
    val lu = new ParamLookup(ps.recs)
    
    // This one should be in the lookup
    lu("leafLength") should be ("0.01")
    
    // But this one should not be and querying it should provoke an error
    intercept[NoSuchElementException] {
      lu("coffee")
    }
  }
  
  it should "assume default units for parameters where none are specified" in {
    val lu = new ParamLookup( ParamsNoUnits.recs, allowDuplicates = true )
    
    // check that the Map within the ParamLookup object has 
    // input units equal to the default
    lu.lookup.values.foreach { rec => 
      val pu = ParamUnitsLookup(rec.label)
      rec.inputUnits should be (pu.defaultInputUnits)
    }
  }
  
  it should "correctly convert between default input units and model units" in {
    val lu = new ParamLookup( ParamsNoUnits.recs, allowDuplicates = true )

    ParamUnitsTable foreach { pu =>
      if (pu.modelUnits != Units.Unitless) {
        val res = lu.dval(pu.label)
        val expected = Units.convert(pu.defaultInputUnits, lu(pu.label).toDouble, pu.modelUnits)
        res should be (expected +- Tol)
      }
    }
  }
  
  it should "correctly assign units for parameters when provided" in {
    val ps = Array(
      Array("NA", "NA", "windSpeed", "5.0", "m/s"),    // default units are km/h
      Array("NA", "NA", "fuelLoad", "1.8", "kg/m2"),   // default units are t/ha
      Array("NA", "NA", "fireLineLength", "0.1", "km") // default units are m
    )
    
    val lu = new ParamLookup(ps.recs)

    // check that the Map within the ParamLookup object has input units
    // correctly set to those provided
    ps foreach { rec =>
      val lurec = lu.lookup( munge(rec(LabelCol)) )
      lurec.inputUnits should be (Units.getUnit(rec(UnitsCol)))
    }
  }
  
}
