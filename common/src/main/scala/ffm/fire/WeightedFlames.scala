package ffm.fire

import ffm.forest.StratumLevel
import ffm.geometry.Coord

/**
 * This object acts as a namespace for the data classes and trait
 * related to calculating weighted flame attributes from ignition paths
 * for species within a stratum.
 */
object WeightedFlames {

  /**
   * Defines the interface for classes which calculate weighted 
   * flame attributes given a set of ignition paths for species
   * within a stratum, and a plant flame model.
   */
  trait Calculator {
    def run(paths: IndexedSeq[IgnitionPath], level: StratumLevel, flameModel: PlantFlameModel): Result
  }

  /**
   * Data class for result to be derived by a [[Calculator]] from a set of
   * ignition paths.
   */
  case class Result(ignitionTime: Double, timeToLongestFlame: Double, params: IndexedSeq[FlameParams]) {

    /** Checks if any weighted attributes exist. */
    def isEmpty = params.isEmpty

    /** Number of weighted attribute ([[FlameParams]]) data. */
    def size = params.size
  }
  
  /**
   * Data class for attributes for which weighted values
   * are calculated
   */
  case class FlameParams(length: Double, depth: Double, origin: Coord, temperature: Double)

  /** A pre-cooked empty result object for convienience. */
  val EmptyResult = Result(0.0, 0.0, Vector.empty)

}

