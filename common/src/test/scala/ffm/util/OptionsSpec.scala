package ffm.util

import ffm.BasicSpec

class OptionsSpec extends BasicSpec {

  "Options.all" should "be true when all args have values" in {
    Options.all(Some(1), Some(2), Some(3)) should be (true)
  }
  
  it should "be false if any args are None" in {
    Options.all(Some(1), None, Some(3)) should be (false)
  }
  
  it should "be false if there are no args" in {
    Options.all() should be (false)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  "Options.any" should "be true when at least one arg has a value" in {
    Options.any(None, None, Some(3)) should be (true)
  }
  
  it should "be false when no args have values" in {
    Options.any(None, None, None) should be (false)
  }
  
  it should "be false when there are no args" in {
    Options.any() should be (false)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  "Options.exactly" should "be true when exactly n args have values" in {
    Options.exactly(2, None, Some(2), None, Some(4)) should be (true)
  }

  it should "be false when less than n args have values" in {
    Options.exactly(2, None, None, None, Some(4)) should be (false)
  }
  
  it should "be false when more than n args have values" in {
    Options.exactly(2, Some(1), Some(2), None, Some(4)) should be (false)
  }
  
  it should "be false when there are less than n args" in {
    Options.exactly(2, Some(1)) should be (false)
  }
  
  it should "be true when there are no args and n = 0" in {
    Options.exactly(0)
  }
  
  /////////////////////////////////////////////////////////////////////////////
  "Options.atLeast" should "be true when exactly n args have values" in {
    Options.atLeast(2, None, Some(2), Some(3)) should be (true)
  }
  
  it should "be true when more than n args have values" in {
    Options.atLeast(1, None, Some(2), Some(3)) should be (true)
  }
  
  it should "be true when n = 0 and no args have values" in {
    Options.atLeast(0, None, None, None) should be (true)
  }

  it should "be false when less than n args have values" in {
    Options.exactly(2, None, None, Some(3)) should be (false)
  }
  
  it should "be false when there are less than n args" in {
    Options.atLeast(2, Some(1)) should be (false)
  }
  
  it should "be true when there are no args and n = 0" in {
    Options.atLeast(0) should be (true)
  }

  /////////////////////////////////////////////////////////////////////////////
  "Options.count" should "return the correct count when all args have values" in {
    Options.count(Some(1), Some(2), Some(3)) should be (3)
  }
  
  it should "return the correct count when some args have values" in {
    Options.count(Some(1), None, Some(3)) should be (2)
  }
  
  it should "return 0 when no args have values" in {
    Options.count(None, None, None) should be (0)
  }
  
  it should "return 0 when there are no args" in {
    Options.count() should be (0)
  }
}