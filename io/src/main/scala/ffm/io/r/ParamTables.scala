package ffm.io.r


/**
 * This object provides for implicit conversion from Array[Array[String]]
 * (the format of tabular data passed from R) to the class ParamTable which
 * provides convenient methods to retrieve and query parameter data.
 */
object ParamTables {
  
  val StratumCol = 0
  val SpeciesCol = 1
  val LabelCol = 2
  val ValueCol = 3
  val UnitsCol = 4
  
  case class ParamTableRec(stratum: String, species: String, paramLabel: String, paramValue: String, paramUnits: String) {
    val isMeta = isNA(stratum)
    val isStratum = !isNA(stratum)
    val isStratumMeta = isStratum && isNA(species)
    val isSpecies = isStratum && !isNA(species)
    
    def toArray: Array[String] = Array(stratum, species, paramLabel, paramValue, paramUnits)
  }
  
  /**
   * Tests if a string value should be treated as NA (missing).
   */
  def isNA(s: String, allowBlank: Boolean = true): Boolean = munge(s) match {
    case "" => allowBlank
    case "na" => true
    case _ => false
  }

  /**
   * Converts a string to lower case and removes all white-space characters.
   */
  def munge(s: String): String = s.toLowerCase.replaceAll("\\s+", "")

  
  implicit class ParamTable(inTbl: Array[Array[String]]) {
    val nrows = inTbl.size
    require(nrows > 0, "Input table should not be empty")
    
    val ncols = inTbl(0).size
    require(ncols == 4 || ncols == 5, s"Input table should have 4 or 5 columns: got $ncols")
    
    require( inTbl.forall( _.size == ncols ), "Input tables has rows with differing number of columns" )
    
    val hasUnitsProvided = ncols == 5
    
    val recs: IndexedSeq[ParamTableRec] = (inTbl map { rec => 
      if (hasUnitsProvided) 
        ParamTableRec(rec(StratumCol), rec(SpeciesCol), rec(LabelCol), rec(ValueCol), rec(UnitsCol))
      else
        ParamTableRec(rec(StratumCol), rec(SpeciesCol), rec(LabelCol), rec(ValueCol), "NA")
    }).toIndexedSeq
    
    
    val metaRecs: IndexedSeq[ParamTableRec] = recs filter (_.isMeta)
    
    val stratumIds: Set[String] = recs.filter(_.isStratum).map( _.stratum ).toSet
    
    def stratumRecs(stratumId: String) = recs.filter( _.stratum == stratumId )
    
    def stratumMetaRecs(stratumId: String) = stratumRecs(stratumId).filter( _.isStratumMeta )
    
    def speciesIds(stratumId: String): Set[String] = stratumRecs(stratumId).filter(_.isSpecies).map( _.species ).toSet
    
    def speciesRecs(speciesId: String) = recs.filter( _.species == speciesId )
  }
  
  
  // Sometimes we want to convert a sequence of ParamTableRecs to a ParamTable.
  // This hack is very inefficient but doesn't get called much.
  def asParamTable(recs: Seq[ParamTableRec]): ParamTable =
    new ParamTable( ( recs.map(_.toArray) ).toArray )
  
}