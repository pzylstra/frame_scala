package ffm

import scala.util.{ Failure, Success }
import ffm.io._
import ffm.fire._
import ffm.util.FileUtils
import ffm.forest.StratumLevel

object TestRig {

  val defaultInPath = "c:/michael/coding/cpp/forest_flammability_model/2.txt"
  val defaultOutDir = "c:/michael/coding/ffm/testing"
  
  def main(args: Array[String]) {

    val path = if (args.length == 1) args(0) else defaultInPath
    val modelDef = ParamFileParser.readTextFormatFile(path).get

    // get fallback value for dead leaf moisture from the surface 
    // dead fuel moisture parameter
    val deadFuelMoisture = new ValueAssignments(modelDef.params).dval("surface dead fuel moisture content")
    val fallback = FallbackProvider(Map("deadLeafMoisture" -> deadFuelMoisture))

    // create the site
    val site = SingleSiteFactory.create(modelDef, fallback) match {
      case Success(site) => site
      case Failure(t) => throw t
    }

    // run the fire model
    val pathModel = new SpikeIgnitionPathModel
    
    // TODO: read fire line length from parameters file (check with Phil if it ever changes)
    val fireModel = new SingleSiteFireModel(pathModel, DefaultPlantFlameModel)(_, _, _) //(site, includeCanopy=true, fireLineLength=100.0)

    val result1 = fireModel(site, true, 100.0).run()
    
    val output1 = ResultFormatter.format(result1)
    
    val outPath = {
      import FileUtils._
      for {
        inName <- fileName(path)
        outName = removeExtension(inName) + "_out_scala.txt"
      } yield makePath(defaultOutDir, outName)
    }
    
    // If there was a canopy stratum with fire spread between crowns, re-run with
    // includeCanopy = false (for altered wind speed calculation)
    val fireSpreadInCanopy = result1.paths exists { path => 
      path.hasIgnition &&
      path.context.stratumLevel == StratumLevel.Canopy &&
      path.context.runType == IgnitionRunType.StratumRun
    }

    val output =
      if (fireSpreadInCanopy) {
        val result2 = fireModel(site, false, 100.0).run()

        val output2 = ResultFormatter.format(result2)

        output1 + "\n\n==== Second run ====\n\n" + output2
      } else output1
    
    if (outPath.isDefined) 
      FileUtils.withPrintWriter(outPath.get) { writer => writer.println(output) }

    println(output)
    
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
  def format(fmr: FireModelResult): String = {
    if (fmr.paths.isEmpty) "No ignition paths"
    else {
      val buf = new StringBuilder
      fmr.paths foreach { path => buf ++= formatPath(path) + '\n' }

      buf.toString
    }
  }

  def formatPath(path: IgnitionPath): String = {
    val buf = new StringBuilder
    val ctxt = path.context

    def add(s: String) = buf ++= s + '\n'

    add(s"${ctxt.stratumLevel} ${path.speciesComponent}")

    add(s"Best result for initial point ${path.initialPoint}")

    val preHeating = path.preIgnitionData.filter(_.isInstanceOf[PreHeatingDrying])
    if (!preHeating.isEmpty) {
      add(PreIgnitionFormatter.preHeatingDryingHeader)
      preHeating foreach { pid =>
        add(PreIgnitionFormatter(pid))
      }
    }

    val incident = path.preIgnitionData.filter(_.isInstanceOf[IncidentDrying])
    if (!incident.isEmpty) {
      add(PreIgnitionFormatter.incidentDryingHeader)
      incident foreach { pid =>
        add(PreIgnitionFormatter(pid))
      }
    }

    if (path.hasIgnition) {
      add("\nStartX\tStartY\tEndX\tEndY\tSegLen")

      path.segments foreach { seg =>
        add(f"${seg.start.x}%.2f\t${seg.start.y}%.2f\t${seg.end.x}%.2f\t${seg.end.y}%.2f\t${seg.length}%.2f")
      }
    }

    buf.toString
  }
}