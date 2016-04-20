package ffm.io.legacy

import scala.io.{BufferedSource, Source}
import scala.util.Try
import java.net.URL


object ParamFileParser extends ExpressionParser {
  
  /** 
   *  Reads text format parameters from the given file path. 
   */
  def readTextFormatFile(path: String): Try[ModelDef] = 
    for {
       src <- Try( Source.fromFile(path) )
       modelDef <- readTextFormat(src)
    } yield modelDef
  
    
  /** 
   *  Reads text format parameters from the given URL. 
   */
  def readTextFormatFile(url: URL): Try[ModelDef] =
    for {
       src <- Try( Source.fromURL(url) )
       modelDef <- readTextFormat(src)
    } yield modelDef
  

  /**
   * Helper function that reads from the input source and builds the ModelDef object.
   */
  private def readTextFormat(src: BufferedSource): Try[ModelDef] = {
    for {
      txt <- Try(src.getLines.toVector)
      ast <- textToAST(txt)
      modelDef <- Try( ASTParser.parseAST(ast) )
    } yield modelDef  }

    
  /**
   * Helper function which takes the input text and builds the AST.
   */
  private def textToAST(lines: Vector[String]): Try[AST] = {
    import ExpressionParser._
    
    val notEmpty = (s: String) => !s.trim().isEmpty()
    
    for {
      exprs <- Try( lines filter notEmpty map parseExpression )
    } yield AST(exprs)
  }
  
}



