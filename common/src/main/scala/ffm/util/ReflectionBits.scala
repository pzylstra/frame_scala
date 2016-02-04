package ffm.util

import scala.reflect.runtime.universe._

/**
 * Used as a mix-in for classes which need to query other class or object
 * methods via reflection.
 * 
 * The stuff in here is black magic.
 */
trait ReflectionBits {
  
  /** 
   * Tests if a given Type has a companion object.
   * 
   * Note: this will be true for all case classes which have a 
   * compiler generated companion object.
   * 
   * Example:
   * {{{
   * import scala.reflect.runtime.universe._
   * 
   * val tt = typeOf[Species]
   * if (hasCompanion(tt)) {
   *   // do something clever
   * }
   * }}}
   * 
   */
  def hasCompanion[T](implicit tt: TypeTag[T]): Boolean =
    tt.tpe.companion != NoType

  /**
   * Searches for a Type's companion object and retrieves the
   * argument names and types for the `apply` methods.
   * 
   * Returns a List of Lists (one for each `apply` method) containing
   * tuples of (String, String) for argument name and type name.
   * 
   * If no companion exists returns an empty list.
   * 
   * Example:
   * {{{
   * val args = companionApplyArgs[Species]
   * }}}
   */
  def companionApplyArgs[T](implicit tt: TypeTag[T]): List[List[(String, String)]] = {
    val result = tt.tpe.companion.decls.collect {
      case m: MethodSymbol if m.name.toString == "apply" =>
        (m.paramLists.map(plist => plist.map(p => (p.name.toString, p.typeSignature.toString)))).flatten
    }
    
    result.toList
  }                     
  
}