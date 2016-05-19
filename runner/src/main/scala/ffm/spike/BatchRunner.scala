package ffm.spike

import scala.util.Failure
import scala.util.Success

import ffm.fire._
import ffm.io.legacy._
import ffm.runner.Runner
import ffm.util.FileUtils
import ffm.forest.Site
import ffm.io.r.Database
import ffm.util.Units

object BatchRunner {
  
  val paramDir = "c:/michael/coding/ffm/params"
  val paramExt = "txt"
  
  val paramFilesSubset = List("93a.txt")
  
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
      case Failure(t) => throw t
    }

    site
  }
    
}


object PreIgnitionFormatter {
  val preHeatingDryingHeader =
    "Length\tDepth\tDist\tTemp\tDrying\tDuration"

  val incidentDryingHeader =
    "Length\tDepth\tDist\tTemp\tDrying\tIDT"

  def apply(data: PreIgnitionData): String = data match {
    case phd: PreHeatingDrying =>
      f"${phd.flameLength}%.2f\t${phd.depth}%.2f\t${phd.distanceToFlame}%.2f\t${phd.dryingTemperature}%.2f\t" +
        f"${phd.dryingFactor}%.4f\t${phd.duration}%.2f"

    case id: IncidentDrying =>
      f"${id.flameLength}%.2f\t${id.depth}%.2f\t${id.distanceToFlame}%.2f\t${id.dryingTemperature}%.2f\t" +
        f"${id.dryingFactor}%.4f\t${id.ignitionDelayTime}%.2f"
  }

}

object ResultFormatter {
  
  def adder(buf: StringBuilder): (String => Unit) = 
    s => buf ++= s + '\n'
  
  def format(fmr: FireModelResult): String = {
    val buf = new StringBuilder
    val add = adder(buf)
    
    add( formatRunResult(fmr.resWithCanopyEffect) )
    
    if (!fmr.resWithoutCanopyEffect.pathsAndFlames.isEmpty) {
      add( "\n\n=== Second run ===\n\n" )
      add( formatRunResult(fmr.resWithoutCanopyEffect, Some(fmr.canopyROS)) )
    }
    
    buf.toString
  }
    
  def formatRunResult(runResult: FireModelRunResult, canopyROS: Option[Double] = None): String = {
    val buf = new StringBuilder    
    val add = adder(buf)
    
    buf ++= formatSurfaceParams(runResult.surfaceOutcome) + '\n'
    
    runResult.flameSummaries foreach { case(level, fsum) =>
      add( s"$level" )
      add( f"  flame length: ${fsum.flameLength}%.2f" )
      add( f"  flame angle:  ${fsum.flameAngle}%.2f" )
      add( f"  flame height:  ${fsum.flameHeight}%.2f" )
    }
    
    add("\nRates of spread")
    runResult.ratesOfSpread foreach { case(level, ros) =>
      val kph = Units.convert("m/s", ros, "km/h")
      add(f"$level : ${ros}%.2f m/s  ${kph}%.2f km/h")
    }
    if (canopyROS.isDefined) {
      val ros = canopyROS.get
      val kph = Units.convert("m/s", ros, "km/h")
      add(f"Canopy : ${ros}%.2f m/s  ${kph}%.2f km/h")
    }
    add("")

    runResult.pathsAndFlames foreach { case (level, pnf) => buf ++= formatPNF(pnf) + '\n' }
    
    buf.toString
  }
  
  def formatSurfaceParams(surf: SurfaceOutcome): String = {
    val buf = new StringBuilder
    val add = adder(buf)
    
    add( f"surface wind speed   ${surf.windSpeed}%.2f" )
    
    add( f"surface flame length ${surf.flameSummary.flameLength}%.2f" )
    add( f"surface flame angle  ${surf.flameSummary.flameAngle}%.2f" )
    add( f"surface flame height ${surf.flameSummary.flameHeight}%.2f" )

    buf.toString
  }

  def formatPNF(pnf: StratumPathsFlames): String = {
    val buf = new StringBuilder
    val add = adder(buf)
   
    pnf.plantPaths foreach (path => add(formatPath(path)))
    pnf.stratumPaths foreach (path => add(formatPath(path)))
    
    buf.toString
  }
    
    
  def formatPath(path: IgnitionPath): String = {
    val buf = new StringBuilder
    val ctxt = path.context
    
    val add = adder(buf)

    add(s"${ctxt.stratumLevel} ${ctxt.runType}: ${path.speciesComponent.species.name}")

    add(s"Best result for initial point ${path.initialPoint}")

    val preHeating = path.preIgnitionData.filter(_.isInstanceOf[PreHeatingDrying])
    if (!preHeating.isEmpty) {
      add("")
      add(PreIgnitionFormatter.preHeatingDryingHeader)
      preHeating foreach { pid =>
        add(PreIgnitionFormatter(pid))
      }
    }

    val incident = path.preIgnitionData.filter(_.isInstanceOf[IncidentDrying])
    if (!incident.isEmpty) {
      add("")
      add(PreIgnitionFormatter.incidentDryingHeader)
      incident foreach { pid =>
        add(PreIgnitionFormatter(pid))
      }
    }

    if (path.hasIgnition) {
      add("\nTime\tStartX\tStartY\tEndX\tEndY\tSegLen")

      path.segments foreach { seg =>
        add(f"${seg.timeStep}\t${seg.start.x}%.2f\t${seg.start.y}%.2f\t${seg.end.x}%.2f\t${seg.end.y}%.2f\t${seg.length}%.2f")
      }
    }

    buf.toString
  }
}
