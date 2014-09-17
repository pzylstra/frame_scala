package ffm.io

/**
 * Defines the traits and classes which represent parameter elements
 * parsed from an input source.
 *
 * This was originally a trait to be mixed in to parser classes, but at present
 * is an object to avoid pesky this.Expression vs that.Expression type errors.
 */
object ExpressionSyntax {

  import annotation.implicitNotFound

  /**
   * Type class to restrict expression values to acceptable types:
   * Int, Double, String.
   */
  @implicitNotFound("No member of ValueLike is available for type ${A}")
  trait ValueLike[A]

  object ValueLike {
    implicit object IntIsValue extends ValueLike[Int]
    implicit object DoubleIsValue extends ValueLike[Double]
    implicit object StringIsValue extends ValueLike[String]
  }

  sealed trait Expression

  case object NoExpression extends Expression

  abstract class NumericLiteral[T: Numeric] extends Expression {
    def value: T
    private def num = implicitly[Numeric[T]]
    def toInt = num.toInt(value)
    def toDouble = num.toDouble(value)
  }

  case class Comment(value: String) extends Expression

  case class IntegerLiteral(value: Int) extends NumericLiteral[Int]
  case class DoubleLiteral(value: Double) extends NumericLiteral[Double]

  sealed trait NumericRange extends Expression
  case class IntegerRange(mean: Int, range: Int) extends NumericRange
  case class DoubleRange(mean: Double, range: Double) extends NumericRange

  sealed trait TextLiteral extends Expression
  case class Text(value: String) extends TextLiteral
  case class ListText(values: List[Text]) extends TextLiteral

  sealed trait Assignment extends Expression { def varName: Text }
  case class ValueAssignment(varName: Text, value: Expression) extends Assignment
  case class EmptyAssignment(varName: Text) extends Assignment

  /**
   * Convenience function to test whether an arbitrary Expression is a NumericLiteral
   * with missing (-99) value.
   */
  def isMissingNumericLiteral(expr: Expression): Boolean =
    expr.isInstanceOf[NumericLiteral[_]] && expr.asInstanceOf[NumericLiteral[_]].toInt == -99
}  
