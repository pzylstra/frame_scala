package ffm.io

import ExpressionSyntax._

/**
 * Abstract syntax tree of Expressions produced by parsing parameter text.
 */
case class AST(expressions: IndexedSeq[Expression]) {

  /** Parameters (all expressions other than comments). */
  def parameters = expressions filter (!_.isInstanceOf[Comment])

  override def toString = s"AST(${expressions.size} expressions)"
}
