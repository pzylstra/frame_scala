package ffm.runner

import scala.util.Failure
import scala.util.Success

import ffm.fire._
import ffm.forest._
import ffm.io.legacy._
import ffm.util.FileUtils


object Runner {
  def run(site: Site): FireModelResult = {
    // run the fire model
    val pathModel = new DefaultIgnitionPathModel
    val plantFlameModel = DefaultPlantFlameModel
    val windModel = DefaultVegetationWindModel

    DefaultSingleSiteFireModelRunner.run(pathModel, plantFlameModel, windModel)(site)    
  }
}

