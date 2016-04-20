package ffm.io.legacy

import scala.util.Try
import ffm.forest.DefaultStratum
import ffm.forest.SpeciesComponent
import ffm.forest.Stratum
import ffm.forest.StratumLevel

object StratumFactory {

  import ExpressionSyntax._
  import FactoryItem._
  
  val items = List(
    item("level", "level"),
    item("plant separation", "plantSeparation")
  )
  
  def create(stratumDef: StratumDef, fallback: FallbackProvider): Try[Stratum] = {
    for {
      vas <- Try( new ValueAssignments(stratumDef.params, items, fallback) )
      sppCompositions <- buildSpeciesComponents(stratumDef, fallback)
      stratum <- buildStratum(vas, sppCompositions)
    } yield stratum
  }
  
  def buildSpeciesComponents(stratumDef: StratumDef, fallback: FallbackProvider): Try[List[SpeciesComponent]] = {
    val tries = stratumDef.species map (spDef => SpeciesComponentFactory.create(spDef, fallback))
    Try( tries map (_.get) )
  }
  
  def buildStratum(vas: ValueAssignments, spp: List[SpeciesComponent]): Try[Stratum] = {
    for {
      level <- Try( StratumLevel(vas.str("level")) )
      plantSeparation <- Try( vas.dval("plantSeparation") )
      stratum <- Try( DefaultStratum(level=level, plantSeparation=plantSeparation, speciesComp=spp) )
    } yield stratum
  }
}