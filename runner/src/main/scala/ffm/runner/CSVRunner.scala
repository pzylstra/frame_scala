package ffm.runner

import com.github.tototoshi.csv.CSVReader

import ffm.fire.FireModelResult
import ffm.io.r.Database
import ffm.io.r.ObjectFactory

object CSVRunner {
  case class CmdLineArgs(paramPath: String, dbPath: String, dbRecreate: Boolean) 
  
  def main(args: Array[String]) {
    val cmd = 
      if (args.isEmpty)
        println("Usage: ffm.runner.CSVRunner -p parampath -d dbpath [-x]")
      else
        doRun(args)
  }
  
  def doRun(args: Array[String]): Unit = {
    val cmd = parseArgs(args)
    
    Database.create(cmd.dbPath, cmd.dbRecreate, useTransactions = false) match {
      case Some(db) => 
        val res = loadAndRun(cmd.paramPath)
        db.insertResult(res)
        db.close()
        println("Success: run completed")
        
      case None =>
        println("Failure: unable to open database " + cmd.dbPath)
    }
  }
  
  def parseArgs(args: Array[String]): CmdLineArgs = {
    val params = args(args.indexOf("-p") + 1)
    val db = args(args.indexOf("-d") + 1)
    val dbRecreate = args.indexOf("-x") != -1
    
    CmdLineArgs(params, db, dbRecreate)
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