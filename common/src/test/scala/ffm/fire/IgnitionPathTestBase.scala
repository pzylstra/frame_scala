package ffm.fire

import org.mockito.Mockito.when
import org.scalacheck.Gen

import ffm.MockSpec
import ffm.forest.Site
import ffm.forest.Species
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.geometry.Coord

/**
 * Base class for IgnitionPathSpec and IgitionPathBuilderSpec classes.
 */
abstract class IgnitionPathTestBase extends MockSpec {

val species = mock[Species]
  val spComp = SpeciesComponent(species, 1.0)
  val stratumLevel = StratumLevel.Canopy
  val site = mock[Site]
  
  val context = mock[IgnitionContext]
  when (context.site) thenReturn site
  when (context.stratumLevel) thenReturn stratumLevel

  // This extends the IgntionPathBuilder with a method to 
  // add segments directly
  implicit class IgnitionPathBuilderEx(builder: IgnitionPathBuilder) {
    def addSegment(s: IgnitedSegment): Unit =
      builder.addSegment(s.timeStep, s.start, s.end)
  }

  def newBuilder() = IgnitionPathBuilder(context, spComp, c0)
  
  val c0 = Coord.Origin 

  /*
   * Creates a Coord with the given X value and Y=0
   */
  def atX(x: Double) = c0.toOffset(x, 0.0)  
  
  /*
   * Generator for lists of random distance values drawn from [minDist, maxDist], 
   * with list sizes will be drawn from [1, maxN].
   */
  def distanceLists(minDist: Double, maxDist: Double, maxN: Int = 10): Gen[List[Double]] = {
    require(minDist < maxDist)
    require(maxN > 1)
    
    for {
      n <- Gen.choose(1, maxN)
      distances <- Gen.listOfN(n, Gen.choose(minDist, maxDist))
    } yield distances
  }
  
  /*
   * Generator for Coords with X and Y values in [-5.0, 5.0].
   */
  val smallCoords: Gen[Coord] =
    for {
      x <- Gen.choose(-5.0, 5.0)
      y <- Gen.choose(-5.0, 5.0)
    } yield Coord(x, y)
    
    
  /**
   * Creates an ignition path based on a sequence of tuples for 
   * time, x0 and x1 of segments.
   * 
   * {{{
   * val path = makePath(
   *   (1, 0, 0.5),
   *   (2, 0.5, 1.0),
   *   (3, 0.5, 1.0),
   *   (4, 1.0, 1.1) )
   * }}}
   */
  def makePath(segmentParams: (Int, Double, Double)* ): IgnitionPath = {
    val b = newBuilder()
    segmentParams foreach { case (t, x0, x1) => b.addSegment(t, atX(x0), atX(x1)) }
    b.toIgnitionPath
  }

  /**
   * Takes a list of distances and (optionally) a start time and returns
   * a list of tuples to use as input for the makePath method.
   */
  def makeSegmentParams(distances: Seq[Double], startTime: Int = 1): IndexedSeq[(Int, Double, Double)] = {
    require( !distances.exists(_ < 0.0), "Distances should be zero or positive" )
    val times = (startTime to (startTime + distances.length - 1)).toIndexedSeq
    val xs = distances.scanLeft(0.0)(_ + _)
    val startXs = xs.init
    val endXs = xs.tail

    times.zip(startXs).zip(endXs) map { case ((t, x0), x1) => (t, x0, x1) }
  }
  
}