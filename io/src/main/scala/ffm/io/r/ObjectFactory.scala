package ffm.io.r

import ffm.forest._
import ffm.geometry.CrownPoly
import ffm.util.Units

/**
 * Creates simulation objects from parameter data
 * passed from R or other applications.
 *
 * Parameter data should be provided as four column matrix
 * of strings (Array[Array[String]] in row-major order).
 * Each row (aka record) has columns (in order): 
 * stratum ID, species ID, parameter label, value.
 *
 * Records for site meta-data rows are identified by a "0" value in
 * both the stratum and species columns.
 *
 * Records for stratum meta-data rows are identified by a "0" value
 * for species and non-zero for stratum.
 */
object ObjectFactory {
  
  type ParamTable = Array[Array[String]]
  
  /** Parameter table column for stratum. */
  val StratumCol = 0
  
  /** Parameter table column for species. */
  val SpeciesCol = 1
  
  /** Parameter table column for parameter label. */
  val ParamCol = 2
  
  /** Parameter table column for value. */
  val ValueCol = 3

  
  /** Checks if a parameter record is site meta-data. */
  def isSiteMetaRec(rec: Array[String]): Boolean =
    isZero(rec(StratumCol)) && isZero(rec(SpeciesCol))

  /** Checks if a parameter record is stratum meta-data. */ 
  def isStratumMetaRec(rec: Array[String]): Boolean = 
    !isZero(rec(StratumCol)) && isZero(rec(SpeciesCol))
    
  
  /**
   * Creates a site with vegetation, surface and weather parameters.
   */
  def createSite(params: ParamTable): Site = {
    SingleSite(
      surface = createSurface(params),
      vegetation = createVegetation(params),
      weather = createWeatherModel(params),
      context = createSiteContext(params) )
  }
 
  /**
   * Creates a Surface object. 
   */
  def createSurface(
      params: ParamTable, 
      slopeUnits: String = "deg",
      fuelLoadUnits: String = "t/ha"): Surface = {
    
    val metaRecs = params.filter( isSiteMetaRec )
    
    // create lookup with strict=false to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(metaRecs, strict = false)

    val slope = Units.convert(slopeUnits, "rad", d(lu("slope")))
    val fuelLoad = Units.convert(fuelLoadUnits, "kg/m2", d(lu("fuelLoad")))
    
    Surface(
      slope = slope,  
      deadFuelMoistureProp = d(lu("deadFuelMoistureProp")),
      fuelLoad = fuelLoad,
      meanFuelDiameter = d(lu("meanFuelDiameter")),
      meanFinenessLeaves = d(lu("meanFinenessLeaves")) )
  }
  
  def createWeatherModel(
      params: ParamTable,
      windSpeedUnits: String = "km/h"): WeatherModel = {
    
    val metaRecs = params.filter( isSiteMetaRec )
    
    // create lookup with strict=false to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(metaRecs, strict = false)
    
    val windSpeed = Units.convert(windSpeedUnits, "m/s", d(lu("windSpeed")))
    
    ConstantWeatherModel(temperature = d(lu("temperature")), windSpeed = windSpeed)
  }
  
  def createSiteContext(params: ParamTable): SiteContext = {
    val metaRecs = params.filter( isSiteMetaRec )

    // create lookup with strict=false to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(metaRecs, strict = false)
    
    SiteContext(fireLineLength = d(lu("fireLineLength")))
  }
  
  /**
   * Creates a vegetation object.
   */
  def createVegetation(params: ParamTable): Vegetation =
    DefaultVegetation(strata = createStrata(params), overlaps = createStratumOverlaps(params))

  /**
   * Creates stratum overlaps.
   */
  def createStratumOverlaps(params: ParamTable): Seq[StratumOverlap] = {
    val overlapRecs = params.filter(rec => isSiteMetaRec(rec) && munge(rec(ParamCol)).startsWith("overlap"))
    overlapRecs.map( rec => createStratumOverlap(rec(ValueCol)) )
  }
  
  /**
   * Creates a StratumOverlap object from a string of the form 
   * "levelname1, levelname2, type".
   */
  def createStratumOverlap(term: String): StratumOverlap = {
    val parts = munge(term).split(",")
    val Array(s1, s2, stype) = parts.size match {
      case 3 => parts
      case 2 => parts :+ "automatic"
      case _ => throw new IllegalArgumentException("Invalid overlap descriptor: " + term)
    }

    val level1 = StratumLevel(s1)
    val level2 = StratumLevel(s2)
    val (lower, upper) =
      if (level1 < level2) (level1, level2)
      else (level2, level1)

    val ovtype = StratumOverlapType(stype)

    StratumOverlap(lower, upper, ovtype)
  }
  

  /**
   * Creates strata from a parameter table.
   */
  def createStrata(params: ParamTable): Seq[Stratum] = {
    val stratumRecs = params.filter( rec => !isZero(rec(StratumCol)) )
    val stratumIds = stratumRecs.map( rec => rec(StratumCol) ).toSet
    require(stratumIds.size > 0, "Parameters for one or more strata are required")
    
    stratumIds.map(i => createStratum(params, i)).toSeq
  }
  
  /**
   * Creates a stratum from parameter table records for the given
   * stratum ID.
   */
  def createStratum(params: ParamTable, stratumId: String): Stratum = {
    val recs = params.filter( rec => rec(StratumCol) == stratumId )
    val (metaRecs, spRecs) = recs.partition( rec => isZero(rec(SpeciesCol)) )

    val spp = spRecs.
      groupBy( rec => rec(SpeciesCol) ).
      values.
      map( createSpeciesComponent(_) ).
      toSeq

    val metaLu = new ParamLookup( recs.filter( isStratumMetaRec(_) ) )
    
    DefaultStratum(
        level = StratumLevel(metaLu("levelName")),
        plantSeparation = d(metaLu("plantSeparation")),
        speciesComp = spp)
  }
    
  def createSpeciesComponent(params: ParamTable): SpeciesComponent = {
    val lu = new ParamLookup(params)
    SpeciesComponent(species = createSpecies(lu), weighting = d(lu("composition")))
  }
  
  def createSpecies(params: ParamTable): Species = 
    createSpecies( new ParamLookup(params) )
  
  def createSpecies(lu: ParamLookup): Species = {
    DefaultSpecies(
      name = lu("name"),
      crown = createCrownPoly(lu),
      liveLeafMoisture = d(lu("liveLeafMoisture")),
      deadLeafMoisture = d(lu("deadLeafMoisture")),
      propDead = d(lu("propDead")),
      propSilicaFreeAsh = dopt(lu("propSilicaFreeAsh")),
      ignitionTemp = dopt(lu("ignitionTemp")),
      leafForm = LeafForm(lu("leafForm")),
      leafThickness = d(lu("leafThickness")),
      leafWidth = d(lu("leafWidth")),
      leafLength = d(lu("leafLength")),
      leafSeparation = d(lu("leafSeparation")),
      stemOrder = d(lu("stemOrder")),
      clumpDiameter = d(lu("clumpDiameter")),
      clumpSeparation = d(lu("clumpSeparation")))
  }

  def createCrownPoly(lu: ParamLookup): CrownPoly = {
    CrownPoly(
      hc = d(lu("hc")),
      he = d(lu("he")),
      ht = d(lu("ht")),
      hp = d(lu("hp")),
      w = d(lu("w")))
  }
  

  def d(x: String) = x.toDouble

  def dopt(x: String) = x match {
    case "NA" => None
    case _    => Some(x.toDouble)
  }
  
  def isZero(s: String): Boolean = munge(s) == "0"

  def munge(term: String): String = term.toLowerCase.replaceAll("\\s+", "")

  
  /**
   * Provides case and white-space insensitive look-up of parameter
   * names.
   * 
   * If strict is true, checks that there were no duplicate parameter labels
   * in the input table.
   */
  class ParamLookup(tbl: ParamTable, strict: Boolean = true) {
    val nrow = tbl.size
    val ncol = if (nrow == 0) 0 else tbl(0).size
    
    val lu = tbl.map(prow => (munge(prow(ParamCol)) -> prow(ValueCol))).toMap
    if (strict) require(lu.size == nrow, "Duplicate parameter labels in input table") 
    
    /** Look-up a parameter name and return the corresponding value. */
    def apply(term: String): String = lu( munge(term) )
  }
}
