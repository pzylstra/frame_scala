package ffm.forest

import scala.util.Failure
import scala.util.Success
import org.mockito.Mockito._
import ffm.MockSpec
import ffm.io.FallbackProvider
import ffm.io.ParamFileParser
import ffm.io.SingleSiteFactory
import ffm.io.ValueAssignments

class VegetationWindModelSpec extends MockSpec {

  "VegetationWindModel" should "calculate the correct wind reduction factor" in {
    
    // lookup of filename -> expected WRF
    val referenceValues = Map(
      "1.txt" -> 2.07,
      "2.txt" -> 5.46,
      "3.txt" -> 2.17,
      "4.txt" -> 1.35,
      "5.txt" -> 3.43,
      "6.txt" -> 17.30)

      val Tol = 0.005
      
    referenceValues foreach { case (file, expectedValue) => 
      val site = loadFileAndCreateSite(file)
      VegetationWindModel.windReductionFactor(site) should be (expectedValue +- Tol)
    }
  }
  
  it should "return 1.0 for wind reduction factor when wind speed is zero" in {
    val site = mock[Site]
    when(site.windSpeed) thenReturn (0.0)
    
    VegetationWindModel.windReductionFactor(site) should be (1.0)
  }
  
  
  /*
   * Helper to load files and create sites.
   * Assumes input files are in resources/ffm/io
   */
  def loadFileAndCreateSite(path: String): Site = {
    val resPath = "/ffm/io/" + path
    val url = getClass.getResource(resPath)
    val modelDef = ParamFileParser.readTextFormatFile(url).get

    // get fallback value for dead leaf moisture from the surface 
    // dead fuel moisture parameter
    val deadFuelMoisture = new ValueAssignments(modelDef.params).dval("surface dead fuel moisture content")
    val fallback = FallbackProvider(Map("deadLeafMoisture" -> deadFuelMoisture))

    // create the site
    SingleSiteFactory.create(modelDef, fallback) match {
      case Success(site) => site
      case Failure(t) => fail(t.getMessage())
    }
  }

}