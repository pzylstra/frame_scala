package ffm.forest

import ffm.geometry.CrownPoly

sealed trait StratumLevel
object StratumLevel {
  case object Surface extends StratumLevel
  case object NearSurface extends StratumLevel
  case object Elevated extends StratumLevel
  case object MidStorey extends StratumLevel
  case object Canopy extends StratumLevel  

  /**
   * Retrieve a StratumLevel by name.
   * 
   * Ignores case and any surrounding or embedded spaces.
   */
  def apply(name: String): StratumLevel = name.replaceAll("\\s+", "").toLowerCase() match {
    case "surface" => Surface
    case "nearsurface" => NearSurface
    case "elevated" => Elevated
    case "midstorey" => MidStorey
    case "canopy" => Canopy
    case s => throw new IllegalArgumentException("Not a valid stratum level name: " + s)
  }}

class Stratum private (
  val level: StratumLevel,
  speciesComps: Vector[SpeciesComposition],
  val plantSep: Double) {
  
  require(!speciesComps.isEmpty, "one or more SpeciesCompositions required")
  require(plantSep >= 0, "plant separation must be >= 0")
  
  /**
   * Species compositions with values normalized to proportion within stratum.
   */
  val speciesCompositions = {
    val sumVals = (speciesComps map (_.composition)).sum
    speciesComps.map( sc => SpeciesComposition(sc.species, sc.composition / sumVals) )
  }
  
  /** Weighted average crown width. */
  val averageWidth = wtAv( sc => crown(sc).width )

  /** Weighted average crown height. */
  val averageTop = wtAv( sc => crown(sc).top )
  
  /** Weighted average crown base. */
  val averageBottom = wtAv( sc => crown(sc).bottom )
  
  /** Weighted average crown mid-height. */
  val averageMidHeight = wtAv( sc => (crown(sc).top + crown(sc).bottom) / 2 )
  
  /** Weighted average flame duration. */
  val averageFlameDuration = wtAv( sc => sc.species.flameDuration )
    
  /** Modelled plant separation. */
  val modelPlantSep = math.max(plantSep, averageWidth)
  
  /** Modelled canopy cover. */
  val cover = { val p = averageWidth / modelPlantSep; p * p }
  
  /** Modelled leaf area index. */
  val leafAreaIndex = cover * wtAv( sc => sc.species.leafAreaIndex )
  
  
  override def toString = 
    s"Stratum($level with ${speciesCompositions.length} species)"
  
  
  /////////////////////////////////////////////////////////
  // Private helper functions
  /////////////////////////////////////////////////////////
  
  /**
   * Extracts the crown polygon from a SpeciesComposition.
   */
  private def crown(sc: SpeciesComposition) = sc.species.crown
  
  /** 
   *  Calculates composition-weighted average of a species attribute.
   *  
   *  Function f extracts the attribute value from each SpeciesComposition.
   */
  private def wtAv(f: SpeciesComposition => Double): Double = {
    (speciesCompositions map (sc => sc.composition * f(sc)) ).sum
  }
  
}

object Stratum {
  
  /**
   * Creates a Stratum object with a vector of SpeciesCompositions.
   */
  def apply(level: StratumLevel, speciesComp: Seq[SpeciesComposition], plantSep: Double): Stratum =
    new Stratum(level, speciesComp.toVector, plantSep)
  
  /**
   * Creates a Stratum object with a single Species.
   */
  def apply(level: StratumLevel, species: Species, plantSep: Double): Stratum =
    new Stratum(level, Vector(SpeciesComposition(species, 1.0)), plantSep)
}