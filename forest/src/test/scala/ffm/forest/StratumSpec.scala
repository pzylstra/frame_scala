package ffm.forest

import org.mockito.Mockito.when
import ffm.MockSpec
import ffm.geometry.CrownPoly

class StratumSpec extends MockSpec {

  val Tol = 1.0e-8

  "A Stratum" should "normalize its input species weighting values" in {

    val N = 10
    val scs = (for {
      i <- 1 to 10
      sp = mockSpecies("sp" + i)
      compVal = math.random * 100
    } yield SpeciesComponent(sp, compVal)).toVector
    
    val stratum = Stratum(StratumLevel.Canopy, scs, 0.0)
    
    val normalizedCompositions = stratum.speciesComponents map (_.weighting)
    normalizedCompositions.sum should be (1.0 +- Tol)
  } 
  
  def mockSpecies(name: String) = {
    val sp = mock[Species]
    
    when(sp.name) thenReturn (name)
    when(sp.crown) thenReturn (CrownPoly(hc=10.0, he=10.0, ht=20.0, hp=20.0, w=10.0))
    sp
  }
  
}