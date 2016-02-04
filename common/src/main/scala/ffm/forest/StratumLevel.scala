package ffm.forest

/**
 * Base class for stratum level objects.
 * 
 * See companion object for details.
 */
sealed abstract class StratumLevel(index: Int) extends Ordered[StratumLevel] {
  def toInt: Int = index
  
  override def compare(that: StratumLevel): Int = this.toInt - that.toInt
}

/**
 * Defines objects used to identify stratum levels.
 * 
 * Objects support ordering as in these examples:
 * {{{
 * import ffm.forest.StratumLevel._
 * 
 * val level1 = Canopy
 * val level2 = MidStorey
 * val higher = level1 > level2  // will be true
 * 
 * val sorted =  TreeSet(Canopy, NearSurface, MidStorey, Elevated)
 * 
 * sorted foreach println  // prints NearSurface
 *                         //        Elevated
 *                         //        MidStorey
 *                         //        Canopy
 * }}}
 */
object StratumLevel {
  case object Surface extends StratumLevel(0)
  case object NearSurface extends StratumLevel(1)
  case object Elevated extends StratumLevel(2)
  case object MidStorey extends StratumLevel(3)
  case object Canopy extends StratumLevel(4)  

  /**
   * Retrieve a StratumLevel by name.
   * 
   * Ignores case and any surrounding or embedded spaces and hyphens.
   * 
   * {{{
   * val level1 = StratmLevel("mid-storey")
   * val level2 = StratumLevel("MidStorey")
   * 
   * level1 == level2  // will be `true`
   * }}}
   */
  def apply(name: String): StratumLevel = name.replaceAll("""[\s\-]+""", "").toLowerCase() match {
    case "surface" => Surface
    case "nearsurface" => NearSurface
    case "elevated" => Elevated
    case "midstorey" => MidStorey
    case "canopy" => Canopy
    case s => throw new IllegalArgumentException("Not a valid stratum level name: " + s)
  }
}

