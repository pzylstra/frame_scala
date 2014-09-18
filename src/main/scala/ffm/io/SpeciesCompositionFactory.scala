package ffm.io

import ffm.forest.{ Species, SpeciesComposition }
import scala.util.Try
import ffm.geometry.CrownPoly
import ffm.forest.LeafForm

object SpeciesCompositionFactory {

  import ExpressionSyntax._
  import FactoryItem._

  val items = List(
    item("composition", "composition"),
    item("name", "name"),
    item("live leaf moisture", "liveLeafMoisture"),
    item("dead leaf moisture", "deadLeafMoisture", optional = true),
    item("silica free ash content", "propSilicaFreeAsh", optional = true),
    item("ignition temperature", "ignitionTemp", optional = true),
    item("proportion dead", "propDead"),
    item("leaf form", "leafForm"),
    item("leaf thickness", "leafThickness"),
    item("leaf width", "leafWidth"),
    item("leaf length", "leafLength"),
    item("leaf separation", "leafSeparation"),
    item("stem order", "stemOrder"),
    item("clump separation", "clumpSeparation"),
    item("clump diameter", "clumpDiameter"),
    item("he", "he"),
    item("ht", "ht"),
    item("hc", "hc"),
    item("hp", "hp"),
    item("w", "w"))

    
  /**
   * Creates a SpeciesComposition object from a SpeciesDef and optional fallback parameters.
   * 
   * @param speciesDef species definition from a [[ModelDef]] object
   * @param fallbacks a FallbackProvider to query for parameters not found in the speciesDef
   */
  def create(speciesDef: SpeciesDef, fallbacks: FallbackProvider): Try[SpeciesComposition] = {
    for {
      vas <- Try(new ValueAssignments(speciesDef.params, items, fallbacks))
      species <- Try(buildSpecies(vas))
    } yield species
  }

  private def buildSpecies(vas: ValueAssignments): SpeciesComposition = {

    val sp = Species(
      name = vas.str("name"),
      crown = CrownPoly(hc = vas.dval("hc"), he = vas.dval("he"), ht = vas.dval("ht"), hp = vas.dval("hp"), w = vas.dval("w")),
      liveLeafMoisture = vas.dval("liveLeafMoisture"),
      deadLeafMoisture = vas.dval("deadLeafMoisture"),
      propDead = vas.dval("propDead"),
      propSilicaFreeAsh = vas.optd("propSilicaFreeAsh"),
      ignitionTemp = vas.optd("ignitionTemp"),
      leafForm = LeafForm(vas.str("leafForm")),
      leafThickness = vas.dval("leafThickness"),
      leafWidth = vas.dval("leafWidth"),
      leafLength = vas.dval("leafLength"),
      leafSeparation = vas.dval("leafSeparation"),
      stemOrder = vas.dval("stemOrder"),
      clumpDiameter = vas.dval("clumpDiameter"),
      clumpSeparation = vas.dval("clumpSeparation"))

    SpeciesComposition(sp, composition = vas.dval("composition"))
  }

}