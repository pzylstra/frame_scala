package ffm.io

import scala.util.Try

import ffm.forest.SingleSite
import ffm.forest.Site
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.forest.StratumOverlap
import ffm.forest.StratumOverlapType
import ffm.forest.Vegetation

object SingleSiteFactory {

  import FactoryItem._

  def create(modelDef: ModelDef, fallback: FallbackProvider): Try[Site] = {
    for {
      weatherModel <- ConstantWeatherModelFactory.create(modelDef)
      surface <- SurfaceFactory.create(modelDef)

      strata <- buildStrata(modelDef, fallback)
      overlaps <- Try(loadOverlaps(modelDef))
      veg <- Try(Vegetation(strata, overlaps))

    } yield SingleSite(surface = surface, vegetation = veg, weather = weatherModel)
  }

  private def buildStrata(modelDef: ModelDef, fallback: FallbackProvider): Try[List[Stratum]] = {
    val tries = modelDef.strata map (sdef => StratumFactory.create(sdef, fallback))

    // This incantation converts `tries` from a List[Try[Stratum]] to a Try[List[Stratum]]
    Try(tries map (_.get))
  }

  private def loadOverlaps(modelDef: ModelDef): List[StratumOverlap] = {
    import ExpressionSyntax._
    
    val ovParams = modelDef.params collect { 
      case param: ValueAssignment if param.varName.value == "overlapping" => param 
    }

    val ovValues = ovParams map {
      case ValueAssignment(name, ListText(values)) =>
        val List(s1, s2, ot) = (values map (_.value))
        
        val level1 = StratumLevel(s1)
        val level2 = StratumLevel(s2)
        
        val (lower, upper) = 
          if (level1 < level2) (level1, level2)
          else (level2, level1)
          
        val ovtype = StratumOverlapType(ot)
        
        StratumOverlap(lower, upper, ovtype)
    }
    
    ovValues
  }
}