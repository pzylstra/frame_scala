package ffm.fire

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

import ffm.ModelSettings
import ffm.numerics.Numerics
import ffm.numerics.Stats

class IgnitionPathSpec extends IgnitionPathTestBase with PropertyChecks {

  val Tol = 1.0e-6

  
  "An IgnitionPath" should "return the correct initial point" in {
    
    forAll(smallCoords) { init => 
      val b = IgnitionPathBuilder(context, spComp, init)
      
      val ip = b.toIgnitionPath.initialPoint      
      ip.almostEq(init) should be (true)
    }
  } 
  
  it should "return the correct max segment length" in {
    
    forAll (distanceLists(minDist=0.0, maxDist=10.0)) { distances =>
    
      val expected = distances.max
      
      val segmentParams = makeSegmentParams(distances)
      val path = makePath( segmentParams: _* )
      
      path.maxSegmentLength should be (expected +- Tol)
    }
  }
  
  
  it should "find the correct earliest time step with max segment length" in {

    forAll (Gen.choose(1, 10), distanceLists(minDist=0.1, maxDist=5.0)) { (startTime, lengths) =>
      
      // Work out the earliest time of the max length
      val maxDist = lengths.max
      val expectedTime = {
        val longestOnes = lengths.zipWithIndex filter { case(d, i) => Numerics.almostEq(d, maxDist) }
        longestOnes.head._2 + startTime
      }
      
      val segmentParams = makeSegmentParams(lengths, startTime)
      val path = makePath( segmentParams: _* )
      path.timeStepForMaxLength should be (expectedTime)
    }
    
  }
  
  
  it should "find the correct time from ignition to max segment length" in {

    forAll (Gen.choose(1, 10), distanceLists(minDist=0.1, maxDist=5.0)) { (ignitionTime, lengths) =>
      
      // Work out the earliest time of the max length
      val maxDist = lengths.max
      val expectedTime = {
        val longestOnes = lengths.zipWithIndex filter { case(d, i) => Numerics.almostEq(d, maxDist) }
        longestOnes.head._2
      }
      
      val segmentParams = makeSegmentParams(lengths, ignitionTime)
      val path = makePath( segmentParams: _* )
      path.timeFromIgnitionToMaxLength should be (expectedTime)
    }
    
  }
  
  it should "return 0.0 for basic rate of spread when ignition did not occur" in {
    val emptyPath = newBuilder().toIgnitionPath
    emptyPath.basicROS should be (0.0)
  }
  
  it should "return 0.0 for basic rate of spread when all segment spreads are below threshold" in {
    val eps = 0.0001
    val dx = (ModelSettings.MinRateForStratumSpread - eps) * ModelSettings.ComputationTimeInterval 
    
    forAll (distanceLists(minDist=0.0, maxDist=dx)) { distances => 
      val segmentParams = makeSegmentParams(distances)
      val path = makePath( segmentParams: _* )
      path.basicROS should be (0.0)
    }    
  }
  
  it should "return the correct basic rate of spread for a single ignited segment" in {
     val minDx = ModelSettings.MinRateForStratumSpread * ModelSettings.ComputationTimeInterval + 0.001
     
     forAll (Gen.choose(minDx, minDx + 10.0)) { x =>
       val path = makePath( (1, 0.0, x) )
       val expectedROS = x / ModelSettings.ComputationTimeInterval 
       path.basicROS should be (expectedROS +- Tol)
     }
  }
  
  it should "return the correct basic rate of spread for multiple segments" in {
    
    forAll (distanceLists(minDist=0.0, maxDist=5.0)) { distances => 
      
      // First calculate expected ROS value from the distances (which will relate to segment Xs)      
      val rates = distances map { _ / ModelSettings.ComputationTimeInterval }
      val ratesFastEnough = rates filter (_ > ModelSettings.MinRateForStratumSpread)
      
      val expectedROS = 
        if (ratesFastEnough.isEmpty) 0.0
        else Stats.mean(ratesFastEnough)
      
      // Now create an ignition path based on the segment distances
      // and compare ROS to expected value
      val segParams = makeSegmentParams(distances)
      val path = makePath( segParams: _* )
      
      path.basicROS should be (expectedROS +- Tol)
    }
  }

}