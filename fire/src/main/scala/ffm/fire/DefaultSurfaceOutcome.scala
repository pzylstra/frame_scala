package ffm.fire

import ffm.ModelSettings._
import ffm.forest.Site
import ffm.forest.StratumLevel
import ffm.forest.VegetationWindModel
import ffm.geometry.Coord

class DefaultSurfaceOutcome(site: Site, windModel: VegetationWindModel, includeCanopy: Boolean) extends SurfaceOutcome {

  /**
   * Wind speed at the surface, derived from ambient wind speed as modified by vegetation.
   */
  val windSpeed = windModel.surfaceWindSpeed(site, includeCanopy)

  private val fireAttr = new DefaultSurfaceFireAttributes(site.surface)
  private val len = fireAttr.flameLength(windSpeed)
  private val angle = DefaultFlame.flameAngle(len, windSpeed, site.surface.slope, site.context.fireLineLength)

  /**
   * Surface flames.
   */
  val flames: IndexedSeq[Flame] = {
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
  
  val flameSummary = StratumFlameSummary(
      StratumLevel.Surface,
      flameLength = len,
      flameAngle = angle,
      flameHeight = len * math.sin(angle) - (len * math.cos(angle)) * math.tan(site.surface.slope) )
      
  val ros: Double = fireAttr.ros(windSpeed)
}

object DefaultSurfaceOutcome {
  /**
   * An empty [[SurfaceOutcome]] object with no flames and zero wind speed.
   */
  object Empty extends SurfaceOutcome {
    val windSpeed = 0.0
    val flames = IndexedSeq.empty[Flame]
    val flameSummary = StratumFlameSummary(StratumLevel.Surface, 0.0, 0.0, 0.0)
    val ros = 0.0
  }
}
