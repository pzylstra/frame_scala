package ffm

import scala.util.{ Failure, Success }
import ffm.io._
import ffm.fire._

object TestRig extends App {

  val path = "c:/michael/coding/cpp/forest_flammability_model/5.txt"
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
  val fireModel = new SingleSiteFireModel(pathModel, DefaultPlantFlameModel)(site, true, 100.0)

  val result = fireModel.run()

  println( ResultFormatter.format(result) )

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

    add( s"${ctxt.stratumLevel} ${path.speciesComponent}" )

    add( s"Best result for initial point ${path.initialPoint}" )

    val preHeating = path.preIgnitionData.filter(_.isInstanceOf[PreHeatingDrying])
    if (!preHeating.isEmpty) {
      add( PreIgnitionFormatter.preHeatingDryingHeader )
      preHeating foreach { pid =>
        add( PreIgnitionFormatter(pid) )
      }
    }

    val incident = path.preIgnitionData.filter(_.isInstanceOf[IncidentDrying])
    if (!incident.isEmpty) {
      add( PreIgnitionFormatter.incidentDryingHeader )
      incident foreach { pid =>
        add( PreIgnitionFormatter(pid) )
      }
    }

    path.segments foreach { seg =>
      add( seg.toString )
    }

    buf.toString
  }
}