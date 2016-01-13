package ffm.numerics

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

import ffm.BasicSpec
import ffm.numerics.Numerics._

class NumericsSpec extends BasicSpec with PropertyChecks {

  val MaxVal: Double = 10000.0
  
  val doubles: Gen[Double] = Gen.choose(-MaxVal, MaxVal)
  
  // Generates values between 0 and 2 * default tolerance
  val tinyValues: Gen[Double] = Gen.choose(0.0, 2 * DefaultTolerance)
  
  // Generates integers 1 or -1
  val sign: Gen[Int] = Gen.oneOf(1, -1)
  
  
  "Numerics" should "determine if values are almost equal" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val expected = tiny <= DefaultTolerance 
      
      Default.almostEq(d, d + tiny) should be (expected)
      Default.almostEq(d, d - tiny) should be (expected)
    }
  }
  
  it should "correctly test tiny values regardless of sign" in {
    forAll (tinyValues, sign) { (tiny, sign) =>
      val expected = tiny < DefaultTolerance 
      
      Default.almostZero(tiny * sign) should be (expected)
    }
  }
  
  it should "test greater than" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isGT = tiny > DefaultTolerance
      
      Default.gt(d + tiny, d) should be (isGT)
    }
  }
  
  it should "test greater than or equal to" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isGEQ = Default.almostZero(tiny) || tiny > DefaultTolerance
      
      Default.geq(d + tiny, d) should be (isGEQ)
    }
  }
  
  it should "test less than" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isLT = tiny > DefaultTolerance
      
      Default.lt(d, d + tiny) should be (isLT)
    }
  }
  
  it should "test less than or equal to" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isLEQ = Default.almostZero(tiny) || tiny > DefaultTolerance
      
      Default.leq(d, d + tiny) should be (isLEQ)
    }
  }
  
  it should "clamp values close to zero regardless of sign" in {
    forAll (tinyValues, sign) { (tiny, sign) =>
      val isZero = tiny < DefaultTolerance
      
      (Default.clampToZero(tiny * sign) == 0.0) should be (isZero)
    }  
  }
  
}