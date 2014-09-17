package ffm.io

import scala.util.Try

import ffm.forest.{Site, SingleSite, Stratum}

object SingleSiteFactory {
  
  import FactoryItem._
  
  def create(modelDef: ModelDef, fallback: FallbackProvider): Try[Site] = {
    for {
      weatherModel <- ConstantWeatherModelFactory.create(modelDef)
      surface <- SurfaceFactory.create(modelDef)
      strata <- buildStrata(modelDef, fallback)
    } yield SingleSite(surface=surface, strata=strata.toVector, weather=weatherModel)
  }
  
  private def buildStrata(modelDef: ModelDef, fallback: FallbackProvider): Try[List[Stratum]] = {
    val tries = modelDef.strata map (sdef => StratumFactory.create(sdef, fallback))
    Try( tries map (_.get) )
  }
  
}