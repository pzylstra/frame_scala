package ffm.io.legacy

import scala.util.Try
import ffm.forest.SingleSite
import ffm.forest.Site
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.forest.StratumOverlap
import ffm.forest.StratumOverlapType
import ffm.forest.DefaultVegetation

object SingleSiteFactory {

  import FactoryItem._

  def create(modelDef: ModelDef, fallback: FallbackProvider): Try[Site] = {
    for {
      weatherModel <- ConstantWeatherModelFactory.create(modelDef)
      surface <- SurfaceFactory.create(modelDef)
      siteContext <- SiteContextFactory.create(modelDef)

      strata <- createStrata(modelDef, fallback)
      overlaps <- Try(loadOverlaps(modelDef))
      veg <- Try(DefaultVegetation(strata, overlaps))

    } yield SingleSite(surface = surface, vegetation = veg, weather = weatherModel, context = siteContext)
  }

  private def createStrata(modelDef: ModelDef, fallback: FallbackProvider): Try[List[Stratum]] = {
    val tries = modelDef.strata map (sdef => StratumFactory.create(sdef, fallback))

    // This incantation converts `tries` from a List[Try[Stratum]] to a Try[List[Stratum]]
    Try(tries map (_.get))
  }

  /**
   * Builds StratumOverlap objects based on input model parameters.
   */
  private def loadOverlaps(modelDef: ModelDef): List[StratumOverlap] = {
    import ExpressionSyntax._

    // Overlap parameters will all have the same name: "overlapping"
    val ovParams = modelDef.params collect {
      case param: ValueAssignment if param.varName.value == "overlapping" => param
    }

    if (ovParams.isEmpty) List.empty
    else {
      ovParams map { case ValueAssignment(_, ListText(items)) =>
          val List(s1, s2, ot) = items map (_.value)

          val level1 = StratumLevel(s1)
          val level2 = StratumLevel(s2)

          val (lower, upper) =
            if (level1 < level2) (level1, level2)
            else (level2, level1)

          val ovtype = StratumOverlapType(ot)

          StratumOverlap(lower, upper, ovtype)
      }
    }
  }
  
}
