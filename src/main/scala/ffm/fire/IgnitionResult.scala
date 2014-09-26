package ffm.fire

import scala.Vector
import ffm.forest.Site
import ffm.forest.Species
import ffm.forest.Stratum
import ffm.forest.StratumLevel
import ffm.geometry.Segment
import ffm.geometry.Coord

/**
 * Stores the results of an ignition run for a given species within stratum with site.
 * 
 * The results include pre-ignition data and ignited segments, if any.
 * 
 * This is a case class with methods supporting builder-style use:
 * {{{
 * // initial empty instance as a var
 * var igResult = IgnitionPath(species, level, site)
 * 
 * // recording ignition time and adding the first ignited segment
 * igResult = igResult.withSegment(2, startCoord, endCoord)
 * 
 * // adding another segment
 * igResult = igResult.withSegment(3. startCoord, endCoord)
 * }}}
 */
case class IgnitionResult(runType: IgnitionRunType, site: Site, level: StratumLevel, species: Species, segments: Vector[IgnitedSegment] = Vector()) {

  /**
   * Returns a new result instance with a segment added.
   * 
   * Throws an error if ignition time has not been set.
   */
  def withSegment(timeStep: Int, start: Coord, end: Coord): IgnitionResult = 
    if (!hasIgnition || timeStep > lastTimeStep.get)
      this.copy(segments = segments :+ new IgnitedSegment(timeStep, start, end))
    else
      throw new Error(s"Time step for ignited segment ($timeStep) should be later than previous time (${lastTimeStep.get})")
  
  def hasIgnition: Boolean =
    !segments.isEmpty
    
  def ignitionTime: Option[Int] =
    segments.headOption map (_.timeStep)
  
  def lastTimeStep: Option[Int] =
    segments.lastOption map (_.timeStep)
    
}


case class IgnitedSegment(timeStep: Int, start: Coord, end: Coord) {
  val length = start.distanceTo(end)
}
