package ffm.forest

import ffm.geometry.Coord
import ffm.numerics.RoundedDoubleSortedSet
import ffm.ModelSettings

/**
 * Represents a site in the modelling landscape having:
 * - an integer identifier
 * - a location
 * - surface characteristics
 * - zero or more vegetation strata
 * - weather
 */
trait Site {
  def id: Long
  def location: Coord
  def surface: Surface
  def strata: Vector[Stratum]
  def weather: WeatherModel
  
  /** Strata index by StatumLevel. */
  val strataByLevel: Map[StratumLevel, Stratum] =
    Map() ++ (strata map (s => (s.level -> s) ))

  /** Gets the slope (shortcut for site.surface.slope). */
  def slope = surface.slope

  /** Gets the ambient temperature (shortcut for site.weather.temperature). */
  def temperature = weather.temperature

  /** Gets the incident windSpeed (shortcut for site.weather.windSpeed). */
  def windSpeed = weather.windSpeed
  
  
  /**
   * Identifies height-delimited layers of uniform vegetation from this site's strata.
   *
   * For example, given a site with three strata:
   * - Canopy 5 - 20m
   * - Mid-storey 2 - 8m
   * - Surface 0 - 1m
   *
   * The vegetation bands would be:
   * - 8 - 20m canopy
   * - 5 - 8m mid-storey, canopy
   * - 2 - 5m mid-storey
   * - 1 - 2m empty layer
   * - 0 - 1m surface
   *
   * If there is no stratum has a lower height at ground level, an empty ground layer
   * will be included.
   *
   * The layers are returned in descending order of height.
   *
   * @param includeCanopy whether to include the canopy (if any) in the returned layers.
   */
  def vegetationLayers(includeCanopy: Boolean): Vector[VegetationLayer] = {
    if (strata.isEmpty) Vector.empty
    else {
      /*
       * Get the average lower and upper height of each stratum and 
       * combine them into a list of unique heights in descending order
       */
      val set = RoundedDoubleSortedSet()
      for (stratum <- strata if includeCanopy || stratum.level != StratumLevel.Canopy) {
        set.add(stratum.averageBottom)
        set.add(stratum.averageTop)
      }
      set.add(0.0) // in case there is no stratum reaching ground level
      val hts = set.toVector.reverse

      /*
       * Examine bands defined by successive height pairs and, for each band,
       * determine the stratum levels within it (if any) and return the results
       * as a Layer object. 
       */
      val layers = for {
        (upperHt, lowerHt) <- hts.zip(hts.tail)

        midHt = (lowerHt + upperHt) / 2
        levels = for {
          stratum <- strata
          if includeCanopy || stratum.level != StratumLevel.Canopy
          if midHt > stratum.averageBottom && midHt < stratum.averageTop
        } yield (stratum.level)

      } yield VegetationLayer(lowerHt, upperHt, levels)

      layers
    }
  }
  
  
  /**
   * Tests if there exists a vertical association between two strata.
   * 
   * FIXME - needs an implementation
   */
  def isVerticalAssociation(level1: StratumLevel, level2: StratumLevel): Boolean = false
}

/**
 * Class for single-site model.
 */
case class SingleSite(surface: Surface, strata: Vector[Stratum], weather: WeatherModel) extends Site {
  val id = 1L
  val location = Coord(0, 0)
}
