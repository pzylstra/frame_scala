package ffm.io.r

import ffm.BasicSpec

class ObjectFactorySpec extends BasicSpec {
  
  val ParamsNoUnits: Array[Array[String]] = readParams("params_no_units.csv")
  
  val Tol = 1.0e-8
  
  import ObjectFactory._
  

  "Simple value searches with ParamLookup" should "be case insensitive" in {
    val ps = Array(
        Array("NA", "NA", "slope", "10"),
        Array("NA", "NA", "deadFuelMoistureProp", "0.05")
        )
        
    val lu = new ParamLookup(ps)
    lu("slope") should be ("10")
    lu("deadFuelMoistureProp") should be ("0.05")
  }
  
  "Double value searches with ParamLookup" should "be case insensitive" in {
    val ps = Array(
        Array("NA", "NA", "slope", "10"),
        Array("NA", "NA", "deadFuelMoistureProp", "0.05")
        )
        
    val lu = new ParamLookup(ps)
    lu.dval("slope") should be (math.toRadians(10.0) +- Tol)
    lu.dval("deadFuelMoistureProp") should be (0.05)
  }
  
  "ParamLookup" should "fail with duplicate parameter labels by default or when strict is true" in {
    val ps = Array(
        Array("NA", "NA", "overlapping", "nearsurface, midstorey"),
        Array("NA", "NA", "overlapping", "midstorey, canopy")
        )
    
    intercept[IllegalArgumentException]{ 
      new ParamLookup(ps, strict = true)
    }
        
    // default (strict == true)
    intercept[IllegalArgumentException]{ 
      new ParamLookup(ps)
    }    
  }
  
  "ParamLookup" should "allow duplicate parameter labels when strict is false" in {
    val ps = Array(
        Array("NA", "NA", "overlapping", "nearsurface, midstorey"),
        Array("NA", "NA", "overlapping", "midstorey, canopy")
        )
    
    new ParamLookup(ps, strict = false)
  }
  
  def readParams(filename: String): Array[Array[String]] = {
    val url = getClass.getResource(filename)
    val lines = scala.io.Source.fromURL(url).getLines()
    
    // drop first line (column names)
    lines.drop(1)
    
    lines.map( line => line.split(",") ).toArray
  }
}