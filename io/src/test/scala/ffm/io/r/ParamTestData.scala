package ffm.io.r

import com.github.tototoshi.csv.CSVReader

object ParamTestData {
  
  import ParamTables._
  
  val ParamsNoUnits: ParamTable = readParams("params_no_units.csv")
  
  def readParams(filename: String): ParamTable = {
    val url = getClass.getResource(filename)
    val src = scala.io.Source.fromURL(url)
    
    val reader = CSVReader.open(src)
    
    val lines = reader.all().
      drop(1).  // drop header line
      map(_.toArray)  // map subsequent lines to Array[String]
    
    reader.close()
    
    // Array[Array[String]] will be implicitly converted to ParamTable
    lines.toArray
  }
  
}