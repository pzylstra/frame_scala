package ffm.fire

import org.mockito.Matchers.any
import org.mockito.Mockito.when
import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks
import scala.util.Random
import ffm.MockSpec
import ffm.forest.Site
import ffm.forest.Species
import ffm.forest.SpeciesComponent
import ffm.forest.StratumLevel
import ffm.geometry.Coord


class DefaultWeightedFlameCalculatorSpec extends MockSpec with PropertyChecks {

  val mockSite = mock[Site]
  val mockSp1 = mock[Species]
  val mockSp2 = mock[Species]

  val FlameLength = 1.0
  val mockFlameModel = mock[PlantFlameModel]
  when(mockFlameModel.flameLength(any[Species], any[Double])) thenReturn (FlameLength)
  
  // Convenient short-cut to calculator function
  val CalcFn = DefaultWeightedFlameCalculator.run(_: IndexedSeq[IgnitionPath], StratumLevel.MidStorey, mockFlameModel)

  // Generators
  val SegmentCounts: Gen[Int] = Gen.choose(1, 10)
  val SpeciesWeights: Gen[Double] = Gen.choose(0.2, 0.8)


  "WeightedFlameAttributes" should "work with an empty collection of ignition paths" in {
    val attr = CalcFn(Vector.empty)
    attr.isEmpty should be(true)
    attr.size should be(0)
  }

  it should "have a size equal to the number of ignited segments in a single path" in {
    val spComp = SpeciesComponent(mockSp1, 1.0)

    forAll(SegmentCounts) { numSeg =>
      val lengths = List.fill(numSeg)(1.0)
      val segments = createSegments(lengths)
      val path = createPath(spComp, StratumLevel.MidStorey)(segments)
      val attr = CalcFn(Vector(path))
      attr.size should be(numSeg)
    }
  }

  it should "have a size equal to the max number of ignited segments over multiple paths" in {
    val spComp = SpeciesComponent(mockSp1, 1.0)
    val pathFn = createPath(spComp, StratumLevel.MidStorey) _

    forAll(SegmentCounts, SegmentCounts, SegmentCounts) { (n1, n2, n3) =>
      val paths = Vector(n1, n2, n3) map (n => pathFn(createUniformSegments(n)))
      val attr = CalcFn(paths)
      attr.size should be(List(n1, n2, n3).max)
    }
  }

  it should "return the correct weighted average time to max flame length" in {
    val level = StratumLevel.MidStorey 
    
    val shortLen = 0.1
    val longLen = 1.0
    
    // function to make a sequence of 0 or more short lengths followed by a single long length
    def makeLenths(numShort: Int) = Vector.fill(numShort)(shortLen) :+ longLen

    /*
     * We generate random weights, ignition times and times from ignition to max length
     * for two species. The calculation of weighted average time to max length should
     * only depend on the weights and the individual times to max length, but we include
     * random ignition time just to test things as per use in simulation.
     */
    forAll(SpeciesWeights, Gen.choose(1, 5), Gen.choose(1, 5), Gen.choose(0, 5), Gen.choose(0, 5)) {
      (wt1, igTime1, igTime2, dt1, dt2) =>
        val wt2 = 1.0 - wt1
        val spComp1 = SpeciesComponent(mockSp1, wt1)
        val spComp2 = SpeciesComponent(mockSp2, wt2)

        // dt1 and dt2 are number of time steps after ignition for max segment length
        val lengths1 = makeLenths(dt1)
        val lengths2 = makeLenths(dt2)

        val path1 = createPath(spComp1, level) {
          createSegments(lengths1, igTime1)
        }

        val path2 = createPath(spComp2, level) {
          createSegments(lengths2, igTime2)
        }

        val attr = CalcFn(Vector(path1, path2))
        attr.timeToLongestFlame should be(dt1 * wt1 + dt2 * wt2)
    }
  }

  def createPath(spComp: SpeciesComponent, level: StratumLevel)(segs: IndexedSeq[IgnitedSegment]): IgnitionPath = {
    val context = createContext(level)
    
    new DefaultIgnitionPath(
      context = context,
      speciesComponent = spComp,
      preIgnitionData = Vector.empty[PreIgnitionData],
      initialPoint = segs.head.start,
      segments = segs)
  }

  def createContext(level: StratumLevel): IgnitionContext = {
    IgnitionContext(
      runType = IgnitionRunType.PlantRun,
      site = mockSite,
      stratumLevel = level,
      preHeatingFlames = Vector(),
      incidentFlames = Vector(),
      preHeatingEndTime = 0.0,
      canopyHeatingDistance = 0.0,
      stratumWindSpeed = 5.0)
  }

  def createUniformSegments(n: Int, length: Double = 1.0, ignitTimeStep: Int = 1) =
    createSegments(Vector.fill(n)(length), ignitTimeStep)
    
  def createSegments(segmentLengths: Seq[Double], ignitTimeStep: Int = 1): IndexedSeq[IgnitedSegment] = {
    val xs = segmentLengths.scanLeft(0.0)(_ + _)
    val starts = xs.init map (x => Coord(x, 0.0))
    val ends   = xs.tail map (x => Coord(x, 0.0))
    
    (0 until segmentLengths.size) map { i =>
      IgnitedSegment(i + ignitTimeStep, starts(i), ends(i)) 
    }
  }
    
}