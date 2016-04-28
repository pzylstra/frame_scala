package ffm.io.r

import ffm.forest._
import ffm.geometry.CrownPoly
import ffm.util.Units

/**
 * Creates simulation objects from parameter data
 * passed from R or other applications.
 *
 * Parameter data should be provided as either a 4 or 5 column table
 * of strings (e.g. an R matrix object) in the form of an Array[Array[String]]
 * object arranged in row-major order. Table columns are (in order):
 *   - stratum identifier
 *   - species identifier
 *   - parameter label
 *   - parameter value
 *   - parameter units (optional) 
 *
 * Rows for site meta-data (e.g. weather variables) are identified by an "NA"
 * value in both the stratum and species columns.
 *
 * Records for stratum meta-data rows are identified by an "NA" value
 * for species and a value other than "NA" for stratum.
 * 
 * If the optional units column is present, each element should be:
 *   - a supported unit abbreviation (see below); or
 *   - blank or "NA" for default units.
 *   
 * 
 */
object ObjectFactory {
  
  import ParamTables._  
  import Units._
  
  /**
   * Class to hold parameter unit information.
   * 
   * @param label parameter label
   * @param defaultInputUnits units assumed on input if not specified by input data
   * @param modelUnits units required by the flammability model
   */
  case class ParamUnits(label: String, defaultInputUnits: Unit, modelUnits: Unit)

  /** 
   * Parameter unit specifiers.
   */
  val ParamUnitsTable = IndexedSeq(
    ("deadFuelMoistureProp",  Unitless,                   Unitless),
    ("fireLineLength",        DistanceMetre,              DistanceMetre),
    ("fuelLoad",              ArealMassTonnesPerHectare,  ArealMassKilogramsPerSquareMetre),
    ("meanFinenessLeaves",    DistanceMetre,              DistanceMetre),
    ("meanFuelDiameter",      DistanceMetre,              DistanceMetre),
    ("overlapping",           Unitless,                   Unitless),
    ("slope",                 AngleDegree,                AngleRadian),
    ("temperature",           TemperatureCelsius,         TemperatureCelsius),
    ("windSpeed",             VelocityKilometresPerHour,  VelocityMetresPerSecond),
    ("clumpDiameter",         DistanceMetre,              DistanceMetre),
    ("clumpSeparation",       DistanceMetre,              DistanceMetre),
    ("composition",           Unitless,                   Unitless),
    ("deadLeafMoisture",      Unitless,                   Unitless),
    ("hc",                    DistanceMetre,              DistanceMetre),
    ("he",                    DistanceMetre,              DistanceMetre),
    ("hp",                    DistanceMetre,              DistanceMetre),
    ("ht",                    DistanceMetre,              DistanceMetre),
    ("ignitionTemp",          TemperatureCelsius,         TemperatureCelsius),
    ("leafForm",              Unitless,                   Unitless),
    ("leafLength",            DistanceMetre,              DistanceMetre),
    ("leafSeparation",        DistanceMetre,              DistanceMetre),
    ("leafThickness",         DistanceMetre,              DistanceMetre),
    ("leafWidth",             DistanceMetre,              DistanceMetre),
    ("liveLeafMoisture",      Unitless,                   Unitless),
    ("name",                  Unitless,                   Unitless),
    ("propDead",              Unitless,                   Unitless),
    ("stemOrder",             Unitless,                   Unitless),
    ("w",                     DistanceMetre,              DistanceMetre),
    ("levelName",             Unitless,                   Unitless),
    ("plantSeparation",       DistanceMetre,              DistanceMetre) ).map( p => ParamUnits.tupled(p) )
    
    
  /** 
   * Provides lookup of units for supported parameters.
   */
  object ParamUnitsLookup {
    val m = Map() ++ (ParamUnitsTable.map { pu => (munge(pu.label), pu) })
    
    /** Retrieves units for a parameter (assumed supported). */
    def apply(param: String): ParamUnits = m( munge(param) )
    
    /** Checks if a parameter is supported. */
    def isRecognizedParam(param: String): Boolean = m.contains( munge(param) )
  }
    
  
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
  def createSurface(params: ParamTable): Surface = {
    // create lookup, allowing it to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(params.metaRecs, allowDuplicates = true)
    
    Surface(
      slope = lu.dval("slope"),
      deadFuelMoistureProp = lu.dval("deadFuelMoistureProp"),
      fuelLoad = lu.dval("fuelLoad"),
      meanFuelDiameter = lu.dval("meanFuelDiameter"),
      meanFinenessLeaves = lu.dval("meanFinenessLeaves") )
  }
  
  def createWeatherModel(params: ParamTable): WeatherModel = {
    // create lookup, allowing it to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(params.metaRecs, allowDuplicates = true)
    
    ConstantWeatherModel(temperature = lu.dval("temperature"), windSpeed = lu.dval("windSpeed"))
  }
  
  def createSiteContext(params: ParamTable): SiteContext = {
    // create lookup, allowing it to ignore duplicate 'overlapping' 
    // parameter labels
    val lu = new ParamLookup(params.metaRecs, allowDuplicates = true)
    
    SiteContext(fireLineLength = lu.dval("fireLineLength"))
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
    val overlapRecs = params.metaRecs.filter( rec => munge(rec.paramLabel) == "overlap" )
    overlapRecs.map( rec => createStratumOverlap(rec.paramValue) )
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
    require(params.stratumIds.size > 0, "Parameters for one or more strata are required")
    
    params.stratumIds.map(id => createStratum(params, id)).toSeq
  }
  
  /**
   * Creates a stratum from parameter table records for the given
   * stratum ID.
   */
  def createStratum(params: ParamTable, stratumId: String): Stratum = {
    val spp = (params.speciesIds(stratumId) map { id =>
      val recs = params.speciesRecs(id)
      createSpeciesComponent( asParamTable(recs) )
    }).toSeq

    val lu = new ParamLookup( params.stratumMetaRecs(stratumId) )
    
    DefaultStratum(
        level = StratumLevel(lu("levelName")),
        plantSeparation = lu.dval("plantSeparation"),
        speciesComp = spp)
  }
    
  def createSpeciesComponent(params: ParamTable): SpeciesComponent = {
    val lu = new ParamLookup(params.recs)
    SpeciesComponent(species = createSpecies(lu), weighting = lu.dval("composition"))
  }
  
  def createSpecies(params: ParamTable): Species = 
    createSpecies( new ParamLookup(params.recs) )
  
  def createSpecies(lu: ParamLookup): Species = {
    DefaultSpecies(
      name = lu("name"),
      crown = createCrownPoly(lu),
      liveLeafMoisture = lu.dval("liveLeafMoisture"),
      deadLeafMoisture = lu.dval("deadLeafMoisture"),
      propDead = lu.dval("propDead"),
      ignitionTemp = lu.dval("ignitionTemp"),
      leafForm = LeafForm(lu("leafForm")),
      leafThickness = lu.dval("leafThickness"),
      leafWidth = lu.dval("leafWidth"),
      leafLength = lu.dval("leafLength"),
      leafSeparation = lu.dval("leafSeparation"),
      stemOrder = lu.dval("stemOrder"),
      clumpDiameter = lu.dval("clumpDiameter"),
      clumpSeparation = lu.dval("clumpSeparation"))
  }

  def createCrownPoly(lu: ParamLookup): CrownPoly = {
    CrownPoly(
      hc = lu.dval("hc"),
      he = lu.dval("he"),
      ht = lu.dval("ht"),
      hp = lu.dval("hp"),
      w = lu.dval("w"))
  }

  
  /**
   * Gets an optional Double value.
   * 
   * Was used for silica free ash content but unused at the moment.
   */
  def dopt(x: String): Option[Double] = 
    if (isNA(x)) None else Some(x.toDouble)
  
  
  /**
   * Provides case and white-space insensitive look-up of parameter
   * names.
   * 
   * If strict is true, checks that there were no duplicate parameter labels
   * in the input table.
   */
  class ParamLookup(tblRecs: IndexedSeq[ParamTableRec], allowDuplicates: Boolean = false) {
    
    // Filter input table to recognized parameters
    val knownRecs = tblRecs filter ( rec => ParamUnitsLookup.isRecognizedParam( rec.paramLabel ) )
    
    case class LookupRec(label: String, inputUnits: Units.Unit, value: String, modelUnits: Units.Unit)

    // For each of the supported input parameters, retrieve unit
    // information by combining default units with any units provided
    // by the caller, then create a Map for look-ups.
    val lookup = {
      val recognizedItems = knownRecs.map { rec =>
        val pu = ParamUnitsLookup(rec.paramLabel)
        
        val inputUnits = 
          if (isNA( rec.paramUnits )) pu.defaultInputUnits
          else Units.getUnit( rec.paramUnits ) 
              
        LookupRec(pu.label, inputUnits, rec.paramValue, pu.modelUnits)  
      }
      
      Map() ++ recognizedItems.map { lurec => (munge(lurec.label) -> lurec) }
    }
        
    if (!allowDuplicates) 
      require(lookup.size == knownRecs.size, "Duplicate parameter labels in input table") 
    
    /** 
     * Looks up a parameter label and returns the corresponding string value. 
     */
    def apply(label: String): String = lookup( munge(label) ).value

    /**
     * Looks up the name of a numeric parameter and returns its value
     * as a Double, taking care of any required unit conversion. 
     */
    def dval(term: String): Double = {
      val lurec = lookup( munge(term) )
      Units.convert(
          fromUnits = lurec.inputUnits, 
          fromValue = lurec.value.toDouble, 
          toUnits = lurec.modelUnits)
    }
      
  }

}
