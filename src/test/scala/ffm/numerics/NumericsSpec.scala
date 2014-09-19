package ffm.numerics

import org.scalacheck.Gen
import org.scalatest.prop.PropertyChecks

import ffm.BasicSpec
import ffm.numerics.Numerics._

class NumericsSpec extends BasicSpec with PropertyChecks {

  val MaxVal: Double = 10000.0
  
  val doubles: Gen[Double] = Gen.choose(-MaxVal, MaxVal)
  
  // Generates values between 0 and 2 * default tolerance
  val tinyValues: Gen[Double] = Gen.choose(0.0, 2 * DefaultTol.value)
  
  // Generates integers 1 or -1
  val sign: Gen[Int] = Gen.oneOf(1, -1)
  
  
  "Numerics" should "determine if values are almost equal" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val expected = tiny <= DefaultTol.value 
      
      almostEq(d, d + tiny) should be (expected)
      almostEq(d, d - tiny) should be (expected)
    }
  }
  
  it should "correctly test tiny values regardless of sign" in {
    forAll (tinyValues, sign) { (tiny, sign) =>
      val expected = tiny < DefaultTol.value 
      
      almostZero(tiny * sign) should be (expected)
    }
  }
  
  it should "test greater than" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isGT = tiny > DefaultTol.value
      
      gt(d + tiny, d) should be (isGT)
    }
  }
  
  it should "test greater than or equal to" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isGEQ = almostZero(tiny) || tiny > DefaultTol.value
      
      geq(d + tiny, d) should be (isGEQ)
    }
  }
  
  it should "test less than" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isLT = tiny > DefaultTol.value
      
      lt(d, d + tiny) should be (isLT)
    }
  }
  
  it should "test less than or equal to" in {
    forAll (doubles, tinyValues) { (d, tiny) =>
      val isLEQ = almostZero(tiny) || tiny > DefaultTol.value
      
      leq(d, d + tiny) should be (isLEQ)
    }
  }
  
  it should "clamp values close to zero regardless of sign" in {
    forAll (tinyValues, sign) { (tiny, sign) =>
      val isZero = tiny < DefaultTol.value
      
      (clampToZero(tiny * sign) == 0.0) should be (isZero)
    }  
  }
  
  "Implicit conversion from Double" should "add an almostEq method" in {
    forAll (doubles) { d =>
      d.almostEq(d + DefaultTol.value / 2) should be (true)
      d.almostEq(d + DefaultTol.value * 2) should be (false)
    }
  }
  
  it should "add an almostZero method" in {
    forAll (tinyValues, sign) { (tiny, sign) =>
      (tiny * sign).almostZero should be (tiny < DefaultTol.value)
    }
    
  }
  
}