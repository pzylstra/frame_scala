package ffm.forest

import ffm.BasicSpec

class LeafFormSpec extends BasicSpec {

  "LeafForm apply method" should "recognize flat" in {
    LeafForm("flat") should be (LeafForm.Flat)
  }
  
  it should "recognize round" in {
    LeafForm("round") should be (LeafForm.Round)
  }
  
  it should "ignore case" in {
    LeafForm("FlaT") should be (LeafForm.Flat)
  }
  
  it should "ignore leading and trailing space" in {
    LeafForm("  flat  ") should be (LeafForm.Flat)
  }
  
  it should "throw an exception for an invalid name" in {
    intercept[IllegalArgumentException] {
      LeafForm("pointy")
    }
  }
}