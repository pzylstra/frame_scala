package ffm.io.legacy

import ffm.BasicSpec

class ExpressionParserSpec extends BasicSpec {
  
  import ExpressionParser._
  import ExpressionSyntax._
  
  val parseNumber = parseFrom(numericLiteral, _: String)

  "An ExpressionParser" should "parse an unsigned integer literal" in {
    parseNumber("42") should be (IntegerLiteral(42))
  }
  
  it should "parse a negative integer literal" in {
    parseNumber("-42") should be (IntegerLiteral(-42))
  }
  
  it should "parse an unsigned floating-point literal" in {
    parseNumber("42.0") should be (DoubleLiteral(42.0))
  }
  
  it should "parse a negative floating-point literal" in {
    parseNumber("-42.0") should be (DoubleLiteral(-42.0))
  }
  
  it should "parse an integer value in exponential format" in {
    parseNumber("1e5") should be (IntegerLiteral(100000))
  }
  
  it should "parse a double value in exponential format" in {
    parseNumber("1.e5") should be (DoubleLiteral(100000.0))
    parseNumber("1e20") should be (DoubleLiteral(1e20))
  }
  
  it should "parse a comma-delimited pair of doubles as a double range" in {
    parseFrom(numericRange, "1.0, 2.0") should be ( DoubleRange( 1.0, 2.0 ) )    
  }
  
  it should "parse a comma-delimited pair of ints as an integer range" in {
    parseFrom(numericRange, "3, 4") should be ( IntegerRange( 3, 4 ) )    
  }
  
  it should "parse a comma-delimited int and double as a double range" in {
    parseFrom(numericRange, "5, 6.0") should be ( DoubleRange( 5.0, 6.0 ) )
  }
  
  it should "parse a text string containing a single word" in {
    val s = "composition"
    parseExpression(s) should be {
      Text(s)
    }
  }
  
  it should "parse a text string containing words delimited by spaces" in {
    val s = "live leaf moisture"
    parseExpression(s) should be {
      Text(s)
    }
  }
  
  it should "parse an assignment expression with a single numeric value" in {
    val varName = "clump diameter"
    val value = 1.0
    
    parseExpression(varName + " = " + value) should be {
      ValueAssignment(Text(varName), DoubleLiteral(1.0))
    }
  }
  
  it should "parse an assignment expression with a numeric range" in {
    val varName = "leaf width"
    val values = List(2.0, 1.0)
    
    parseExpression(varName + " = " + values.mkString(", ")) should be {
      ValueAssignment(
          Text(varName), 
          DoubleRange(2.0, 1.0))
    }
  }
  
  it should "parse an assignment expression with a text string value" in {
    val varName = "name"
    val value = "Eucalyptus macrorhyncha"
    
    parseExpression(varName + " = " + value) should be {
      ValueAssignment(Text(varName), Text(value))
    }
  }
  
  it should "parse an assignemnt expression with a comma-delimited list of words" in {
    val varName = "overlapping"
    val values = List("near surface", "elevated", "automatic")
    
    parseExpression(varName + " = " + values.mkString(", ")) should be {
      ValueAssignment(Text(varName), ListText( values map Text ))
    }
  }
  
  it should "parse an empty assignment expression (no rhs values)" in {
    val varName = "silica free ash content"
    
    parseExpression(varName + " = ") should be {
      EmptyAssignment(Text(varName))
    }
  }
  
  it should "parse a comment line" in {
    val line = "# this is a comment"
      
    parseExpression(line) should be ( Comment(line) )
  }

}