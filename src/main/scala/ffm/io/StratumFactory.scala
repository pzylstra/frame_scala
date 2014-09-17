package ffm.io

import scala.util.Try
import ffm.forest.{Stratum, SpeciesComposition, StratumLevel}

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
      sppCompositions <- buildSpeciesCompositions(stratumDef, fallback)
      stratum <- buildStratum(vas, sppCompositions)
    } yield stratum
  }
  
  def buildSpeciesCompositions(stratumDef: StratumDef, fallback: FallbackProvider): Try[List[SpeciesComposition]] = {
    val tries = stratumDef.species map (spDef => SpeciesCompositionFactory.create(spDef, fallback))
    Try( tries map (_.get) )
  }
  
  def buildStratum(vas: ValueAssignments, spp: List[SpeciesComposition]): Try[Stratum] = {
    for {
      level <- Try( StratumLevel(vas.str("level")) )
      plantSeparation <- Try( vas.dval("plantSeparation") )
      stratum <- Try( Stratum(level=level, plantSep=plantSeparation, speciesComp=spp) )
    } yield stratum
  }
}