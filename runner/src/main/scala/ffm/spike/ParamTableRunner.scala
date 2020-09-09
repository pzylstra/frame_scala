package ffm.spike

import com.github.tototoshi.csv.CSVReader
import ffm.io.r.ObjectFactory
import ffm.runner.Runner
import ffm.fire.FireModelResult

/**
 * Test rig to run the model with a table of parameters read
 * from a CSV file.
 */
object ParamTableRunner {

  val defaultPath = "c:/michael/Rworkspaces/Phil/package_testing/Final1.csv"

  def main(args: Array[String]) {
    val modelResult =
      if (args.isEmpty)
        // println("usage: ParamTableRunner filename")
        loadAndRun(defaultPath)
      else
        loadAndRun(args(0))
        
    println( ResultFormatter.format(modelResult) )
  }

  def loadAndRun(path: String): FireModelResult = {
    val tbl: Array[Array[String]] = loadTable(path)
    val site = ObjectFactory.createSite(tbl)

    Runner.run(site)
  }

  def loadTable(path: String): Array[Array[String]] = {
    val src = io.Source.fromFile(path)
    val reader = CSVReader.open(src)

    val lines = reader.all().
      drop(1). // drop header row
      map(_.toArray) // map subsequent lines to Array[String]

    reader.close()

    lines.toArray
  }
}