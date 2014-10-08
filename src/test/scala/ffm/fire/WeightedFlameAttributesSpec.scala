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

class WeightedFlameAttributesSpec extends MockSpec with PropertyChecks {

  val mockSite = mock[Site]
  val mockSp1 = mock[Species]
  val mockSp2 = mock[Species]

  val FlameLength = 1.0
  val mockFlameModel = mock[PlantFlameModel]
  when(mockFlameModel.flameLength(any[Species], any[Double])) thenReturn (FlameLength)

  // Generators
  val SegmentCounts: Gen[Int] = Gen.choose(1, 10)
  val SpeciesWeights: Gen[Double] = Gen.choose(0.2, 0.8)

  val AttributesFunc = WeightedFlameAttributes(mockFlameModel) _

  "WeightedFlameAttributes" should "work with an empty collection of ignition paths" in {
    val attr = AttributesFunc(Vector.empty)
    attr.isEmpty should be(true)
    attr.size should be(0)
  }

  it should "have a size equal to the number of ignited segments in a single path" in {
    val spComp = SpeciesComponent(mockSp1, 1.0)

    forAll(SegmentCounts) { numSeg =>
      val segments = createUniformSegments(numSeg)
      val path = createPath(spComp, StratumLevel.MidStorey)(segments)
      val attr = AttributesFunc(Vector(path))
      attr.size should be(numSeg)
    }
  }

  it should "have a size equal to the max number of ignited segments over multiple paths" in {
    val spComp = SpeciesComponent(mockSp1, 1.0)
    val pathFn = createPath(spComp, StratumLevel.MidStorey) _

    forAll(SegmentCounts, SegmentCounts, SegmentCounts) { (n1, n2, n3) =>
      val paths = Vector(n1, n2, n3) map (n => pathFn(createUniformSegments(n)))
      val attr = AttributesFunc(paths)
      attr.size should be(List(n1, n2, n3).max)
    }
  }

  it should "return the correct weighted average time to max flame length (1 segment)" in {
    val level = StratumLevel.MidStorey 
    val igTime1 = 1
    val igTime2 = 5
    
    forAll(SpeciesWeights) { wt1 =>
      val wt2 = 1.0 - wt1
      val spComp1 = SpeciesComponent(mockSp1, wt1)
      val spComp2 = SpeciesComponent(mockSp2, wt2)
      
      val path1 = createPath(spComp1, level) {
        createUniformSegments(1, ignitTimeStep=igTime1)
      }

      val path2 = createPath(spComp2, level) {
        createUniformSegments(1, ignitTimeStep=igTime2)
      }
      
      val attr = AttributesFunc(Vector(path1, path2))
      attr.timeToLongestFlame should be(igTime1 * wt1 + igTime2 * wt2)
    }
  }

  def createPath(spComp: SpeciesComponent, level: StratumLevel)(segs: IndexedSeq[IgnitedSegment]): IgnitionPath = {
    val ctxt = createContext(level)

    val path = new IgnitionPath {
      val context = ctxt
      val preIgnitionData = Vector.empty[PreIgnitionData]
      val initialPoint = segs.head.start
      val segments = segs
      val speciesComponent = spComp
    }

    path
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

  def createUniformSegments(numSegments: Int, length: Double = 1.0, ignitTimeStep: Int = 0): IndexedSeq[IgnitedSegment] = {
    createSegments(numSegments, startFn = i => Coord.Origin, endFn = i => Coord(0.0, length), ignitTimeStep)
  }
  
  def createSegments(numSegments: Int, startFn: (Int) => Coord, endFn: (Int) => Coord, ignitTimeStep: Int): IndexedSeq[IgnitedSegment] =
    (0 until numSegments) map (i => IgnitedSegment(i + ignitTimeStep, startFn(i), endFn(i)))
  
}