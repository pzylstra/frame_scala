package ffm.forest

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar

class SpeciesComponentSpec extends FlatSpec with Matchers with MockitoSugar {

  val species = mock[Species]
  
  "SpeciesComponent" should "throw an exception if weighting is zero" in {
    intercept[IllegalArgumentException] {
      SpeciesComponent(species, 0)
    }
  }
  
  it should "throw an exception if weighting is negative" in {
    intercept[IllegalArgumentException] {
      SpeciesComponent(species, -1)
    }
  }  
  
  it should "correctly report the input composition" in {
    val sc = SpeciesComponent(species, 0.5)
    sc.weighting should be (0.5)
  }
}