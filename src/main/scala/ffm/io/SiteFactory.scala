package ffm.io

import scala.util.Try

import ffm.forest.{Site, SingleSite, Stratum, Vegetation}

object SingleSiteFactory {
  
  import FactoryItem._
  
  def create(modelDef: ModelDef, fallback: FallbackProvider): Try[Site] = {
    for {
      weatherModel <- ConstantWeatherModelFactory.create(modelDef)
      surface <- SurfaceFactory.create(modelDef)
      strata <- buildStrata(modelDef, fallback)
      
      // FIXME - get overlaps from model def
      veg <- Try( Vegetation(strata, Vector()) )
      
    } yield SingleSite(surface=surface, vegetation=veg, weather=weatherModel)
  }
  
  private def buildStrata(modelDef: ModelDef, fallback: FallbackProvider): Try[List[Stratum]] = {
    val tries = modelDef.strata map (sdef => StratumFactory.create(sdef, fallback))
    Try( tries map (_.get) )
  }
  
}