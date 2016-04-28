package ffm.io.r

import ffm.BasicSpec

class ParamTablesSpec extends BasicSpec {
  import ParamTestData.ParamsNoUnits
  import ParamTables._
  
  "ParamTable" should "ignore NA values when retrieving stratum IDs" in {
    val hasNA = ParamsNoUnits.stratumIds.exists { id => isNA(id) }
    hasNA should be (false)
  }
  
  it should "ignore NA values when retrieving species IDs" in {
    ParamsNoUnits.stratumIds foreach { id =>
      val hasNA = ParamsNoUnits.speciesIds(id).exists { id => isNA(id) }
      hasNA should be (false)
    }
  }
}