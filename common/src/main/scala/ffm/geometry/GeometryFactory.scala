package ffm.geometry

import JTSImplicits._
import ffm.ModelSettings.DistancePrecision
import com.vividsolutions.jts.geom.{GeometryFactory, PrecisionModel, Point, LineString, Polygon}
import com.vividsolutions.jts.precision.GeometryPrecisionReducer

object JTSGeometryFactory {
  
  private val scale = math.pow(10, DistancePrecision.toDouble)
  val precModel = new PrecisionModel(scale)
  val factory = new GeometryFactory(precModel)
  
  val reducer = new GeometryPrecisionReducer(precModel)

  def createPoint(coord: Coord): Point = {
    val geom = factory.createPoint(coord)
    reducer.reduce(geom).asInstanceOf[Point]
  }
  
  def createLineString(coords: Seq[Coord]): LineString = {
    val geom = factory.createLineString(coords.toArray)
    reducer.reduce(geom).asInstanceOf[LineString]
  }
  
  def createPolygon(coords: Seq[Coord]): Polygon = {
    val geom = factory.createPolygon(coords.toArray)
    reducer.reduce(geom).asInstanceOf[Polygon]
  }
}