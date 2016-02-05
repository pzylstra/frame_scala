package ffm.io

import ExpressionSyntax._

/**
 * Helper class for factories to read and query a list of ValueAssignments from 
 * the AST (e.g. species parameters).
 * 
 * If a non-empty list of FactoryItems is provided:
 * - an assignment must exist in the input parameters for each item otherwise an error results;
 * - assignments can be retrieved using the item argNames.
 * 
 * If an empty list of FactoryItems is provided:
 * - all value assignments in the input parameters will be available;
 * - assignments can be retrieved using the parameter names.
 */
class ValueAssignments(params: List[Assignment], items: List[FactoryItem], fallbackProvider: FallbackProvider = FallbackProvider.Default) {
  
  /** 
   * Creates an object which will hold all ValueAssignments in the input 
   * parameters, indexed by parameter name.
   */
  def this(params: List[Assignment]) = this(params, List())

  // Values provided by getValueAssignments take precedence over fallbacks
  val assignmentsByArgName: Map[String, ValueAssignment] = fallbackProvider.fallbacks ++ getValueAssignments()

  val valuesByArgName: Map[String, Any] = 
    assignmentsByArgName map { case (argName, as) => (argName, valueAsAny(as)) }

  def get(key: String): Any =
    valuesByArgName.getOrElse(key, throw new Error("key not found: " + key))

  def optd(key: String): Option[Double] = valuesByArgName.get(key) match {
    case Some(d: Double) => Some(d)
    case Some(i: Int) => Some(i.toDouble)
    case None => None
    case Some(other) => throw new Error(s"Invalid value for $key: $other")
  }

  def str(key: String): String = get(key).asInstanceOf[String]

  def dval(key: String): Double = get(key) match {
    case dbl: Double => dbl
    case int: Int => int.toDouble
  }

  def ival(key: String): Int = get(key) match {
    case int: Int => int
    case x => throw new Error("Expected integer value but got " + x)
  }

  private def getValueAssignments() =
    if (items.isEmpty) getAllValueAssignments()
    else getItemValueAssignments()
    
  private def getAllValueAssignments() =
    (for {
      p <- params
      if p.isInstanceOf[ValueAssignment]
      va = p.asInstanceOf[ValueAssignment]
    } yield (p.varName.value, va)).toMap
    
  private def getItemValueAssignments() =   
    (for {
      it <- items
      p <- params find (_.varName.value == it.key)
      if p.isInstanceOf[ValueAssignment]
      va = p.asInstanceOf[ValueAssignment]
    } yield (it.argName, va)).toMap

  private def valueAsAny(v: ValueAssignment): Any = v.value match {
    case IntegerLiteral(i) => i
    case DoubleLiteral(d) => d
    case IntegerRange(mean, _) => mean
    case DoubleRange(mean, _) => mean
    case Text(txt) => txt
    case ListText(txts) => txts.mkString(",") // FIXME !  This is a temp hack for the `overlapping` parameters
    
    case expr => throw new IllegalArgumentException("Invalid value for species parameter " + v)
  }
}