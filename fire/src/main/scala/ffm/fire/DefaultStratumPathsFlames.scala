package ffm.fire

import ffm.forest.Stratum

/**
 * Default companion object for the [[StratumPathsFlames]] trait with methods to create
 * data objects for ignition and non-ignition outcomes.
 */
object DefaultStratumPathsFlames {

  /**
   * Create a StratumOutcome object for results where ignition occurred 
   */
  def ignitionOutcome(stratum: Stratum,
                      plantPaths: IndexedSeq[IgnitionPath],
                      plantFlames: IndexedSeq[Flame],
                      stratumPaths: IndexedSeq[IgnitionPath],
                      stratumFlames: IndexedSeq[Flame]): StratumPathsFlames = {

    require(isIgnition(plantPaths), "Expected plant ignition data")

    require(plantFlames.nonEmpty, "Expected plant flames")

    def fs(flames: IndexedSeq[Flame]): Option[StratumFlameSeries] =
      if (flames.isEmpty) None
      else Some(DefaultStratumFlameSeries(stratum, flames))

    new DefaultImpl(stratum, plantPaths, fs(plantFlames), stratumPaths, fs(stratumFlames))
  }

  /**
   * Create a StratumOutcome object for results where ignition did not occur.
   */
  def nonIgnitionOutcome(stratum: Stratum,
                         plantPaths: IndexedSeq[IgnitionPath]): StratumPathsFlames = {

    require(plantPaths.nonEmpty,
      "Expected one or more plant paths with pre-ignition data")

    require(!isIgnition(plantPaths),
      "Paths should not have ignition data")

    new DefaultImpl(stratum, plantPaths, None, Vector.empty, None)
  }

  // Helper function to look for ignition in a sequence of 
  // IgnitionPath objects
  private def isIgnition(paths: IndexedSeq[IgnitionPath]) =
    paths exists (_.hasIgnition)

  
  /**
   * **************************************************************************
   * Private implementation of StratumOutcome trait
   * **************************************************************************
   */
  private class DefaultImpl(
      val stratum: Stratum,
      val plantPaths: IndexedSeq[IgnitionPath],
      val plantFlameSeries: Option[StratumFlameSeries],
      val stratumPaths: IndexedSeq[IgnitionPath],
      val stratumFlameSeries: Option[StratumFlameSeries]) extends StratumPathsFlames
}
  
