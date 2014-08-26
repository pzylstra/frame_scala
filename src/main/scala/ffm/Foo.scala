package ffm

import com.vividsolutions.jts.algorithm.CGAlgorithms
import ffm.geometry.Coord
import ffm.geometry.JTSImplicits

object Foo extends App {

  import JTSImplicits._
  
  val p1 = Coord(0, 0)
  val p2 = Coord(1000, 1000)
  val q = Coord(1, 1)

  val result = CGAlgorithms.distancePointLine(q, Array(p1, p2))
  println(s"result = $result")
}

