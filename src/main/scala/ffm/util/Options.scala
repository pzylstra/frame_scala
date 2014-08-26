package ffm.util

/**
 * Provides some handy methods for dealing with Option instances.
 */
object Options {

  /**
   * Tests whether any of the Option objects passed as arguments 
   * have a value.
   */
  def any(ops: Option[Any]*): Boolean = {
    ops.exists(_.isDefined)
  }
  
  /**
   * Tests whether all of the Option objects passed as arguments
   * have values.
   */
  def all(ops: Option[Any]*): Boolean = {
    !(ops.isEmpty || ops.exists(_.isEmpty))
  }
  
  /**
   * Tests whether exactly n arguments have values.
   */
  def exactly(n: Int, ops: Option[Any]*): Boolean = {
    count(ops: _*) == n
  }
  
  /**
   * Tests whether at leat n arguments have values.
   */
  def atLeast(n: Int, ops: Option[Any]*): Boolean = {
    count(ops: _*) >= n
  }
  
  /**
   * Counts the number of arguments with values.
   */
  def count(ops: Option[Any]*): Int = {
    ops.count(_.isDefined)
  }
  
}
