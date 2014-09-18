package ffm.io

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator.JavaTokenParsers

/**
 * Defines the grammar for text parameter input.
 */
trait ExpressionParser extends JavaTokenParsers {
  import ExpressionSyntax._
  
  /** Main entry point for the parser. */
  def expression: Parser[Expression] = 
    comment | paramLine
    
  /** Parses a comment. */
  def comment: Parser[Comment] = """\#.*""".r ^^ Comment
  
  /** Parses a parameter line (assignment or word group). */
  def paramLine: Parser[Expression] = 
    assignmentLine | wordsLine
    
  /** Parses a line with an assignment expression and an optional trailing comment. */
  def assignmentLine: Parser[Assignment] =
    assignment <~ opt(comment)
    
  /** Parses a line with a word group and optional trailing comment. */
  def wordsLine: Parser[Text] =
    wordGroup <~ opt(comment)
      
  /** Parses an assignment expression. */
  def assignment: Parser[Assignment] = 
    (wordGroup ~ EQ ~ opt(rhsValues)) ^^ { case txt ~ _ ~ rhs => {
      rhs match {
        // Have to take care of -99 NumericLiterals (yuk)
        case Some(rhs) if isMissingNumericLiteral(rhs) => EmptyAssignment(txt)
        case Some(rhs) => ValueAssignment(txt, rhs) 
        case None => EmptyAssignment(txt)
      }
    }
  }
  
  /** 
   *  Parses the right-hand side of an assignment expression.
   */
  def rhsValues: Parser[Expression] = ((numericRange ||| numericLiteral) | (wordGroup ||| wordList))
  
  /** Parses two or more sets of words delimited by commas. */
  def wordList: Parser[ListText] = 
    wordGroup ~ COMMA ~ rep1sep(wordGroup, COMMA) ^^ { case w1 ~ _ ~ ws => ListText(w1 :: ws) }
  
  /** Parses one or more words delimited by spaces. */
  def wordGroup: Parser[Text] = rep1(word) ^^ { ws => Text(ws.mkString(" ")) }
  
  /** 
   *  Parses a word (text string with no spaces). 
   *  
   *  We allow numbers and a few non-alpha characters to be in a word.
   */
  def word: Parser[String] = """\w[0-9\w\/\-\_]*""".r
    
  /** Parses two numbers delimited by commas representing mean and range (uniform distribution). */ 
  def numericRange: Parser[NumericRange] = 
    numericLiteral ~ COMMA ~ numericLiteral ^^ { case n1 ~ _ ~ n2 => {
      val isD = n1.isInstanceOf[DoubleLiteral] || n2.isInstanceOf[DoubleLiteral]
      if (isD)
        DoubleRange(n1.toDouble, n2.toDouble)
      else 
        IntegerRange(n1.toInt, n2.toInt)
    }
  }
  
  /** Parses a number as either an integer or a double value. 
   */
  def numericLiteral: Parser[NumericLiteral[_]] = floatingPointNumber ^^ { s => 
    val dot = s.contains(".")
    val d = s.toDouble
    if (!dot && d.isValidInt) IntegerLiteral(d.toInt)
    else DoubleLiteral(d)
  }

  def EQ: Parser[String] = "="
    
  def COMMA: Parser[String] = ","
}

/**
 * Companion object with methods to run the expression parser.
 */
object ExpressionParser extends ExpressionParser {
  import ExpressionSyntax._
  
  def parseExpression(s: CharSequence): Expression =
    parseFrom(expression, new CharSequenceReader(s))
    
  def parseFrom(entryPoint: Parser[Expression], s: CharSequence): Expression =
    parseFrom(entryPoint, new CharSequenceReader(s))
    
  def parseFrom(entryPoint: Parser[Expression], input: CharSequenceReader): Expression = 
    phrase(entryPoint)(input) match {
    case Success(expr, _) => expr
    case NoSuccess(msg, rest) => 
      throw new IllegalArgumentException("Failed to parse input near " + rest.pos.longString + ": " + msg)
    }
}
