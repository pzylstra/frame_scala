package ffm.spike

import ffm.forest.Site
import ffm.io.ParamFileParser
import ffm.io.SingleSiteFactory
import ffm.io.ValueAssignments
import scala.util.Failure
import ffm.io.FallbackProvider
import scala.util.Success
import ffm.forest.StratumLevel
import ffm.util.FileUtils
import ffm.fire.DefaultPlantFlameModel
import ffm.fire.DefaultIgnitionPathModel
import ffm.fire.DefaultSingleSiteFireModelRunner
import ffm.forest.DefaultVegetationWindModel

/**
 * Temp hack program used to calculate LAI and ignitability coefficient for each 
 * stratum in a set of sites read from param files in the given folder.
 */
object CalculateVars extends App {
  
  case class Vars(paramFile: String, level: StratumLevel, lai: Double, ignitabilityCoef: Double, flameHeight: Double = 0.0) {
    
    override def toString = {
      val fmt = (d: Double) => "%.4f".format(d)
      paramFile + "," + level + "," + fmt(lai) + "," + fmt(ignitabilityCoef) + "," + fmt(flameHeight)     
    }

  }

  val inDir = "c:/michael/coding/ffm/calculate_vars"
    
  val paramFiles = {
    val f = new java.io.File(inDir)
    require(f.isDirectory)
    
    val files = f.listFiles().toIndexedSeq map (_.getPath())
    files filter (_.endsWith(".txt"))
  }  
  
  val vars = (paramFiles map getVars).flatten
  
  FileUtils.withPrintWriter(inDir + "/vars2.csv"){ writer =>
    writer.println("params,level,lai,ignitCoef")
    vars foreach (v => writer.println(v))
  }

  def getVars(paramPath: String): IndexedSeq[Vars] = {
    println(paramPath)
    System.out.flush()

    val site = makeSite(paramPath)
    val fname = (FileUtils.fileName(paramPath).get).replace(".txt", "")

    val initVars = site.vegetation.strata.map { s =>
      Vars(fname, s.level, s.leafAreaIndex, s.ignitabilityCoef)
    }

    // run the fire model and get flame heights
    val pathModel = new DefaultIgnitionPathModel
    val plantFlameModel = DefaultPlantFlameModel
    val windModel = DefaultVegetationWindModel
    val fireLineLength = 100.0
    val result = DefaultSingleSiteFireModelRunner.run(pathModel, plantFlameModel, windModel)(site, fireLineLength)

    val flameHeightsByLevel = {
      val levelsAndHts = result.stratumResults.map(res => (res.level, res.flameHeight) )
      Map() ++ levelsAndHts
    }
    
    // combine pre-run vars with flame heights
    initVars map { v => 
      val ht = flameHeightsByLevel.getOrElse(v.level, 0.0) 
      v.copy(flameHeight = ht)
    }
  }
  
  def makeSite(paramPath: String): Site = {
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