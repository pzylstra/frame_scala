package ffm.forest

import org.scalatest.FlatSpec
import org.scalatest.Matchers
import org.scalatest.mock.MockitoSugar

class SpeciesCompositionSpec extends FlatSpec with Matchers with MockitoSugar {

  val species = mock[Species]
  
  "SpeciesComposition" should "throw an exception if composition is zero" in {
    intercept[IllegalArgumentException] {
      SpeciesComposition(species, 0)
    }
  }
  
  it should "throw an exception if composition is negative" in {
    intercept[IllegalArgumentException] {
      SpeciesComposition(species, -1)
    }
  }  
  
  it should "correctly report the input composition" in {
    val sc = SpeciesComposition(species, 0.5)
    sc.composition should be (0.5)
  }
}