package ffm.fire

import ffm.geometry.Coord

/**
 * Holds common parameters used to create and describe flames.
 */
case class FlameParams(
  length: Double,
  depth: Double,
  origin: Coord,
  temperature: Double)
