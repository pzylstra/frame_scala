package ffm.io.legacy

import scala.util.Try
import ffm.forest.SiteContext

object SiteContextFactory {

  import ExpressionSyntax._
  import FactoryItem._
  
  val items = List(
    item("fire line length", "fireLineLength")
  )
  
  def create(modelDef: ModelDef): Try[SiteContext] = {
    for {
      vas <- Try( new ValueAssignments(modelDef.params, items) )
      context <- Try( SiteContext(fireLineLength = vas.dval("fireLineLength") ) )
    } yield context
  }
      
}