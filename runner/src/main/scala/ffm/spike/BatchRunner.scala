package ffm.spike

import scala.util.Failure
import scala.util.Success

import ffm.forest.Site
import ffm.io.legacy._
import ffm.io.r.Database
import ffm.runner.Runner
import ffm.util.FileUtils

object BatchRunner {

  // val paramDir = "c:/michael/coding/ffm/params"
  val paramDir = "c:/michael/Rworkspaces/Phil/package_testing"
  val paramExt = "txt"

  val paramFilesSubset = List("site_100_weather_12.txt") // List("93a_wind20_slope02.txt")

  def main(args: Array[String]): Unit = {
    val allFiles = (new java.io.File(paramDir)).listFiles.filter(_.isFile).toList

    val paramFiles =
      if (paramFilesSubset.isEmpty) allFiles
      else allFiles.filter { f => paramFilesSubset.contains(f.getName) }

    paramFiles.foreach { pf =>
      val name = pf.getPath
      if (name.endsWith(paramExt)) {
        println(name)
        loadAndRun(name, dump = true, database = "") //"c:/michael/Rworkspaces/Phil/package/ffm_out.db")
      }
    }
  }

  def loadAndRun(paramPath: String,
                 outputDir: String = "c:/michael/coding/ffm/testing",
                 database: String = "",
                 dump: Boolean = false): Unit = {

    val site = loadParams(paramPath)
    val res = Runner.run(site)

    val resultText = ResultFormatter.format(res)

    if (dump) println(resultText)

    if (database != "") {
      Database.create(database, deleteIfExists = true, useTransactions = false) match {
        case Some(db) =>
          db.insertResult(res)
          db.close()

        case None =>
          println("Could create database")
      }
    }

    val outPath = {
      import FileUtils._
      for {
        inName <- fileName(paramPath)
        outName = removeExtension(inName) + "_out_scala.txt"
      } yield makePath(outputDir, outName)
    }

    if (outPath.isDefined)
      FileUtils.withPrintWriter(outPath.get) { writer => writer.println(resultText) }
  }

  def loadParams(paramPath: String): Site = {
    val modelDef = ParamFileParser.readTextFormatFile(paramPath).get

    // get fallback value for dead leaf moisture from the surface 
    // dead fuel moisture parameter
    val deadFuelMoisture = new ValueAssignments(modelDef.params).dval("surface dead fuel moisture content")
    val fallback = FallbackProvider(Map("deadLeafMoisture" -> deadFuelMoisture))

    // create the site
    val site = SingleSiteFactory.create(modelDef, fallback) match {
      case Success(site) => site
      case Failure(t)    => throw t
    }

    site
  }

}

