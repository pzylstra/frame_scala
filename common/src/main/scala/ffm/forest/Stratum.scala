package ffm.forest

trait Stratum extends Ordered[Stratum] {
  
  /** Level of this stratum. */
  def level: StratumLevel
  
  /** Species within the stratum and their associated weightings (e.g. proportions). */
  def speciesComponents: IndexedSeq[ SpeciesComponent ]
  
  /** Weighted average crown width. */
  val averageWidth: Double

  /** Weighted average crown height. */
  val averageTop: Double
  
  /** Weighted average crown base. */
  val averageBottom: Double
  
  /** Weighted average crown mid-height. */
  val averageMidHeight: Double
  
  /** Plant separation (provided value). */
  def plantSeparation: Double
  
  /** Plant separation (possibly adjusted average value). */
  val modelPlantSeparation: Double
  
  /** Weighted average canopy cover. */
  val cover: Double
  
  /** Weighted average leaf area index. */
  val leafAreaIndex: Double
  
  /** Weighted average of species ignitability coefficients. */
  val ignitabilityCoef: Double
  
}