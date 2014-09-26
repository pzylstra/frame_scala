package ffm.fire

import scala.collection.mutable.ArrayBuffer

import ffm.geometry.Coord

trait IgnitionResult {
  def segments: IndexedSeq[IgnitedSegment]
  
  def hasIgnition: Boolean =
    !segments.isEmpty

  def ignitionTime: Option[Int] =
    segments.headOption map (_.timeStep)

  def lastTimeStep: Option[Int] =
    segments.lastOption map (_.timeStep)
  
}


/**
 * Used to progressively record results during an ignition path run and
 * then provide them as an immutable IgnitionResult object.
 */
trait IgnitionResultBuilder extends IgnitionResult {
  def addSegment(timeStep: Int, start: Coord, end: Coord)
  def toResult: IgnitionResult
}


/**
 * Companion object for the IgnitionResultBuilder trait.
 * 
 * Usage:
 * {{{
 * // At the beginning of an ignition path simulation
 * val resultBuilder = IgnitionResultBuilder(runType, ignitionContext)
 * 
 * // During the simulation
 * resultBuilder.addSegment(timeStep, startPoint, endPoint)
 * 
 * // At the end of the simulation
 * resultBuilder.toResult
 * }}}
 */
object IgnitionResultBuilder {
  def apply(runType: IgnitionRunType, context: IgnitionContext): IgnitionResultBuilder =
    new Builder(runType, context)

  private class Builder(runType: IgnitionRunType, context: IgnitionContext) extends IgnitionResultBuilder {
    private val segmentBuffer = ArrayBuffer.empty[IgnitedSegment]
    
    def segments = segmentBuffer.toVector

    def addSegment(timeStep: Int, start: Coord, end: Coord) {
      if (!hasIgnition || timeStep == lastTimeStep.get + 1)
        segmentBuffer += new IgnitedSegment(timeStep, start, end)
      else
        throw new IllegalArgumentException(
            s"Time step for ignited segment ($timeStep) should be later than previous time (${lastTimeStep.get})")
    }
    
    def toResult = new BasicIgnitionResult(runType, context, segments)

  }
  
  private class BasicIgnitionResult(
    val runType: IgnitionRunType, 
    val context: IgnitionContext, 
    val segments: Vector[IgnitedSegment]) extends IgnitionResult
}


case class IgnitedSegment(val timeStep: Int, val start: Coord, val end: Coord) {
  val length = start.distanceTo(end)

  override def equals(other: Any) = {
    other match {
      case that: ffm.fire.IgnitedSegment => 
        that.canEqual(IgnitedSegment.this) && 
        timeStep == that.timeStep && 
        start.almostEq(that.start) &&
        end.almostEq(that.end)
        
      case _ => false
    }
  }

}



