package ffm.fire

import ffm.forest.Stratum

class StratumOutcome private (
    val stratum: Stratum,
    val plantPaths: IndexedSeq[IgnitionPath],
    val plantFlameSeries: Option[StratumFlameSeries],
    val stratumPaths: IndexedSeq[IgnitionPath],
    val stratumFlameSeries: Option[StratumFlameSeries]) {
  
  
  def selectFlameSeries(f: (StratumFlameSeries, StratumFlameSeries) => StratumFlameSeries) = plantFlameSeries match {
    case Some(plantFS) =>
      stratumFlameSeries match {
        case Some(stratumFS) =>
          Some( f(plantFS, stratumFS) )
          
        case None => 
          Some( plantFS )
      }
      
    // no plant or stratum flames
    case None => None
  }
      
}

object StratumOutcome {
  
  def ignitionOutcome(stratum: Stratum,
    plantPaths: IndexedSeq[IgnitionPath],
    plantFlames: IndexedSeq[Flame],
    stratumPaths: IndexedSeq[IgnitionPath],
    stratumFlames: IndexedSeq[Flame]): StratumOutcome = {

    require(isIgnition(plantPaths), "Expected plant ignition data")
    
    require(plantFlames.nonEmpty, "Expected plant flames")

    def fs(flames: IndexedSeq[Flame]): Option[StratumFlameSeries] =
      if (flames.isEmpty) None
      else Some(new StratumFlameSeries(stratum, flames))

    new StratumOutcome(stratum, plantPaths, fs(plantFlames), stratumPaths, fs(stratumFlames))
  }

  def nonIgnitionOutcome(stratum: Stratum,
      plantPaths: IndexedSeq[IgnitionPath]): StratumOutcome = {
    
    require(plantPaths.nonEmpty,
        "Expected one or more plant paths with pre-ignition data")
        
    require(!isIgnition(plantPaths),
        "Paths should not have ignition data")
    
    new StratumOutcome(stratum, plantPaths, None, Vector.empty, None)
  }
  
  private def isIgnition(paths: IndexedSeq[IgnitionPath]) =
    paths exists (_.hasIgnition)
}
