package ffm.fire

import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord

class DefaultSurfaceOutcome(site: Site, fireLineLength: Double, windModel: VegetationWindModel, includeCanopy: Boolean) extends SurfaceOutcome {

  /**
   * Wind speed at the surface, derived from ambient wind speed as modified by vegetation.
   */
  val windSpeed = windModel.surfaceWindSpeed(site, includeCanopy)

  /**
   * Surface flames.
   */
  val flames: IndexedSeq[Flame] = {
    val fireAttr = new DefaultSurfaceFireAttributes(site.surface)
    val len = fireAttr.flameLength(windSpeed)
    val angle = DefaultFlame.flameAngle(len, windSpeed, site.surface.slope, fireLineLength)
    val ht = len * (math.sin(angle) - math.cos(angle) * math.tan(site.surface.slope))
    
    val nflames = math.round(fireAttr.flameResidenceTime / ComputationTimeInterval).toInt
    
    Vector.fill(nflames) {
      DefaultFlame(
        length = len,
        angle = angle,
        origin = Coord.Origin,
        depthIgnited = 0,
        deltaTemperature = MainFlameDeltaTemperature)
    }
  }
}
