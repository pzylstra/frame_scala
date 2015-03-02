package ffm.fire

import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord

/**
 * Calculates and stores surface-related fire parameters given a site, fire line length
 * and whether the canopy stratum should be included or excluded.
 */
class SurfaceParams(site: Site, fireLineLength: Double, includeCanopy: Boolean) {

  /**
   * Wind speed at the surface, derived from ambient wind speed as modified by vegetation.
   */
  val windSpeed = VegetationWindModel.surfaceWindSpeed(site, includeCanopy)

  /**
   * Surface flame length.
   */
  val flameLength = site.surface.flameLength(windSpeed)

  /**
   * Surface flame angle.
   */
  val flameAngle =
    Flame.flameAngle(flameLength, windSpeed, site.slope, fireLineLength)
    
  /**
   * Surface flame height.
   */
  val flameHeight =
    flameLength * (math.sin(flameAngle) - math.cos(flameAngle) * math.tan(site.slope))    

  /**
   * Surface flames.
   */
  val flames: IndexedSeq[Flame] = {
    val nflames = math.round(site.surface.flameResidenceTime / ComputationTimeInterval).toInt
    Vector.fill(nflames) {
      Flame(
        length = flameLength,
        angle = flameAngle,
        origin = Coord.Origin,
        depthIgnited = 0,
        deltaTemperature = MainFlameDeltaTemperature)
    }
  }
}
