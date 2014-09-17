package ffm.geometry

import com.vividsolutions.jts.{geom => JTS}

import scala.language.implicitConversions

/**
 * Provides implicit conversions to and from mutable JTS classes
 * and corresponding immutable ffm classes.
 */
object JTSImplicits {
  /** Converts a JTS Coordinate to an immutable Coord. */
  implicit def JTSToCoord(c: JTS.Coordinate): Coord =
    Coord(c.x, c.y)
    
  /** Converts an immutable Coord to a JTS Coordinate. */
  implicit def CoordToJTS(coord: Coord): JTS.Coordinate =
    new JTS.Coordinate(coord.x, coord.y)
  
  /** 
   * Converts an array of Coords to one of JTS Coordinates. 
   */
  implicit def CoordArrayToJTSCoordinateArray(coords: Array[Coord]): Array[JTS.Coordinate] = 
    coords map (c => new JTS.Coordinate(c.x, c.y))
  
 }
