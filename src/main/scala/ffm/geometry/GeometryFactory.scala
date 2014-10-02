package ffm.geometry

import com.vividsolutions.jts.geom

object JTSGeometryFactory {
  
  /**
   * A JTS GeometryFactory with default (double) precision for non-robust
   * operations.
   */
  val factory = new geom.GeometryFactory
  
  def apply() = factory
  
}