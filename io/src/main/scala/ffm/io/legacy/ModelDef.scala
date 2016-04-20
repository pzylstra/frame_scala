package ffm.io.legacy

import ExpressionSyntax._

case class SpeciesDef(params: List[Assignment])

case class StratumDef(params: List[Assignment], species: List[SpeciesDef])

case class ModelDef(params: List[Assignment], strata: List[StratumDef]) {
  require(allStrataHaveSpecies && !params.isEmpty)
  
  val numStrata = strata.size
  val numSpecies = (strata map {_.species.size}).sum
  
  override def toString = {
    val desc = 
      (if (numStrata == 1) "1 stratum" else s"$numStrata strata") +
      " and " +
      s"$numSpecies species"
    
    s"ModelDef($desc)"
  }
  
  private def allStrataHaveSpecies: Boolean =
    if (strata.isEmpty) false
    else {
      val nums = strata map (_.species.size)
      !nums.exists(_ == 0)
    }
}

