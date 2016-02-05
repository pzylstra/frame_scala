package ffm.io

import java.io.FileNotFoundException

import org.scalatest.TryValues

import ffm.BasicSpec
import ffm.io.ExpressionSyntax._

class ParamFileParserSpec extends BasicSpec with TryValues {

  "ParamFileParser.readTextFormatFile" should "return Failure when the file is not found" in {
    val result = ParamFileParser.readTextFormatFile("does_not_exist.txt")
    result.failure.exception shouldBe a [FileNotFoundException]
  }
  
  it should "read the correct number of strata and species from a file" in {
    val modelDef = parseFile("56a.txt")
    
    modelDef.numStrata should be (4)
    modelDef.numSpecies should be (5)
  }
  
  it should "read the correct number of model parameters" in {
    val modelDef = parseFile("56a.txt")
    
    modelDef.params.length should be (15)
  }

  /* 
   * Loads and parses a text-format parameters file from the test resources directory.
   */
  def parseFile(filename: String): ModelDef = {
    val url = getClass.getResource(filename)
    val result = ParamFileParser.readTextFormatFile(url)
    result.success.value 
  }
}