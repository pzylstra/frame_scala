package ffm.forest

import ffm.geometry.CrownPoly

trait Species {
  def name: String
  
  def crown: CrownPoly
  
  def liveLeafMoisture: Double
  def deadLeafMoisture: Double
  def propDead: Double
  
  def leafForm: LeafForm
  def leafThickness: Double
  def leafWidth: Double
  def leafLength: Double
  def leafArea: Double
  def leafMoisture: Double
  def leafSeparation: Double
  def leavesPerClump: Double
  def leafAreaIndex: Double
  
  def stemOrder: Double
  def clumpDiameter: Double
  def clumpSeparation: Double

  def ignitionTemperature: Double
  def ignitabilityCoef: Double
}
