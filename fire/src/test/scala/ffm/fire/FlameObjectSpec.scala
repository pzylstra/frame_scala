package ffm.fire

/**
 * Tests for Flame companion object methods.
 */
class FlameObjectSpec extends FlameTestBase {
  
  "Flame" should "return 0 for flame angle when flame length is 0" in {
    forAll (windSpeeds, slopes, distances) { (wind, slope, fireLineLength) =>
      Flame.flameAngle(0.0, wind, slope, fireLineLength) should be (0.0)
    } 
  }

}