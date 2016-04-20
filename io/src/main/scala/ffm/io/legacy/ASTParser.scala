package ffm.io.legacy

import scala.util.parsing.combinator.Parsers
import scala.util.parsing.input.Position
import scala.util.parsing.input.Reader
import scala.language.implicitConversions

import ExpressionSyntax._

trait ASTParser extends Parsers {
  
  type Elem = Expression
  
  def modelDef: Parser[ModelDef] =
    rep1(stratum) ~ rep1(assignment) ^^ { case strata ~ params => ModelDef(params, strata)}
  
  def stratum: Parser[StratumDef] =
    "begin stratum" ~> stratumDef <~ "end stratum"
    
  def stratumDef: Parser[StratumDef] =
    rep1(assignment) ~ rep1(species) ^^ { case params ~ spp => StratumDef(params, spp) }
  
  def species: Parser[SpeciesDef] =
    "begin species" ~> rep1(assignment) <~ "end species" ^^ { as => SpeciesDef( as map (_.asInstanceOf[Assignment]) ) }
  
  def assignment: Parser[Assignment] =
    (valueAssignment | emptyAssignment) ^^ { a => a.asInstanceOf[Assignment] }
  
  def valueAssignment: Parser[Expression] = 
    matchOn( e => e.isInstanceOf[ValueAssignment], "ValueAssignment")
    
  def emptyAssignment: Parser[Expression] =
    matchOn( e => e.isInstanceOf[EmptyAssignment], "EmptyAssignment")
  
  /**
   * Implicit conversion from String to Parser[Expression].
   * 
   * Expects that the current input element is a Text object matching the input string.
   */
  implicit def matchText(value: String): Parser[Expression] = new Parser[Expression] {
    def apply(in: Input) = {
      if (in.atEnd) Failure(s"Expected Text($value) but reached end of AST", in)
      else if (in.first == Text(value)) Success(in.first, in.rest)
      else Failure(s"Expected Text($value) but got " + in.first, in.rest)
    }
  }

  /**
   * Matches on an attribute of the current input element tested by the provided function.
   */
  def matchOn(f: (Expression) => Boolean, typeName: String) = new Parser[Expression] {
    def apply(in: Input) = {
      if (in.atEnd) Failure(s"Expected $typeName but reached end of AST", in)
      else {
        val e = in.first
        if (f(e)) Success(e, in.rest)
        else Failure(s"Expected $typeName but got " + e, in.rest)
      }
    }
  }
}

object ASTParser extends ASTParser {

  def parseAST(ast: AST): ModelDef =
    parseAll(modelDef, new ExpressionReader(ast)) match {
      case Success(md, _) => md
      case NoSuccess(msg, _) => 
        throw new IllegalArgumentException(msg)
    }
  
  private def parseAll[T](p: Parser[T], in: ExpressionReader): ParseResult[T] =
    phrase(p)(in)
  
}


class ExpressionPosition(ast: AST, offset: Int) extends Position {
  /** Column position: always -1 since it is not relevant to AST position. */
  val column: Int = -1
  
  /** Line position: offset in AST.parameters from 0, or -1 for end-of-input. */
  def line: Int = if (offset < ast.parameters.length) offset else -1
    
  /** Line contents: string representation of current expression. */
  protected def lineContents: String = ast.parameters(offset).toString
}


object ExpressionReader {
  val EofAST = NoExpression
}

class ExpressionReader(val ast: AST, override val offset: Int) extends Reader[Expression] {

  import ExpressionReader._

  /** Creates a new ExpressionReader positioned at the start of the AST. */
  def this(ast: AST) = this(ast, 0)

  /** Returns the first element of the reader or EofAST if at the end of the AST. */
  def first: Expression =
    if (!atEnd) ast.parameters(offset)
    else EofAST

  /** Returns a new reader containing all elements except the first. 
   *
   *  Returns `this` if the reader has reached the end of the AST.  
   */
  def rest: ExpressionReader =
    if (!atEnd) new ExpressionReader(ast, offset + 1)
    else this

  /** Returns true if there are no more elements in this reader other than EofAST. */
  def atEnd: Boolean =
    offset >= ast.parameters.length

  /** Returns the position of the reader. */
  def pos: Position = new ExpressionPosition(ast, offset)

}