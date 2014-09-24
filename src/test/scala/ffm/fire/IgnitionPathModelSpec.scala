package ffm.fire

import ffm.BasicSpec
import ffm.forest._
import ffm.geometry.CrownPoly

class IgnitionPathModelSpec extends BasicSpec {

  val poa = Species(
    name = "Poa sieberiana",
    crown = CrownPoly(hc = 0, he = 0, ht = 0.05, hp = 0.1, w = 0.08),
    liveLeafMoisture = 1.0,
    deadLeafMoisture = 0.036,
    propDead = 0.5,
    propSilicaFreeAsh = Some(0.013),
    ignitionTemp = None,
    leafForm = LeafForm.Flat,
    leafThickness = 0.16e-3,
    leafWidth = 1.3e-3,
    leafLength = 150e-3,
    leafSeparation = 3.8e-3,
    stemOrder = 1,
    clumpDiameter = 0.08,
    clumpSeparation = 0)
      
  val surface = Surface(
      slope = 0.0,
      deadFuelMoistureProp = 0.036,
      fuelLoad = 15.4,
      meanFuelDiameter = 5e-3,
      meanFinenessLeaves = 0.49e-3)

  val weather = ConstantWeatherModel(temperature=20.0, windSpeed=30.0)
  
  val strata = Vector(Stratum(StratumLevel.Surface, poa, 0.0))
  
  val veg = Vegetation(strata, Vector())
  
  val site = SingleSite(surface, veg, weather)
  
  "An ignition run" should "do something" in {
    
    /* 
     * 
     * FIXME !
    IgnitionPathModel.plantFlameRun(site, Stratum.Surface, poa, *** further arguments ***)  
    * 
    */  
  }
}