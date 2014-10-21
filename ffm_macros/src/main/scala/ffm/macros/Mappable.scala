package ffm.macros

import scala.language.experimental.macros 
import scala.reflect.macros.blackbox.Context

/**
 * Defines methods for conversion between case class instance and Map of
 * constructor parameters.
 * 
 * The companion object of this trait contains a Scala macro to generate code
 * for case class to/from Map conversion. The macro was developed by 
 * [[http://blog.echo.sh/post/65955606729/exploring-scala-macros-map-to-case-class-conversion Jonathan Chow]].
 * 
 * Example of use:
 * {{{
 * import ffm.macros._
 * import Mappable._
 * 
 * case class Person(name: String, age: Int)
 * 
 * val bob = Person("Bob", 42)
 * val params = mapify(bob)
 * println(params)  // prints Map(name -> Bob, age -> 42)
 * 
 * val bob2 = unmapify[Person](params)
 * println(bob2)   // prints Person(Bob,42)
 * }}}
 */
trait Mappable[T] {
  def toMap(t: T): Map[String, Any]
  def fromMap(map: Map[String, Any]): T
}

/**
 * Companion object to the [[ffm.macros.Mappable]] trait.
 * 
 * Provides an implicit macro to materialize a case class instance from 
 * a Map[String, Any] of constructor parameters. See the trait description for
 * more details.
 */
object Mappable {
  /**
   * Returns a Map[String, Any] of constructor parameters for the given object.
   * 
   * The parameters are taken from the class primary constructor.
   */
  def mapify[T](t: T)(implicit mapper: Mappable[T]) = mapper.toMap(t)
  
  /**
   * Returns an object of type T constructed with parameters in the given Map.
   * 
   * Assumes:
   * - there is a companion object for type T
   * - the companion object has an apply method which takes the same parameters
   *   as the primary constructor for class T
   *   
   * These assumptions will always be satisfied for case classes.
   */
  def unmapify[T](map: Map[String, Any])(implicit unmapper: Mappable[T]) = unmapper.fromMap(map)  
  
  /**
   * Creates a mapper object to convert between class instances of type T and Maps.
   */
  implicit def mapper[T]: Mappable[T] = macro _mapperImpl[T]

  /**
   * The macro implementation to generate code for a mapper object.
   */
  def _mapperImpl[T: c.WeakTypeTag](c: Context) = {
    import c.universe._
    val tpe = weakTypeOf[T]
    val companion = tpe.typeSymbol.companion

    val fields = tpe.decls.collectFirst {
      case m: MethodSymbol if m.isPrimaryConstructor => m
    }.get.paramLists.head

    val (toMapParams, fromMapParams) = fields.map { field =>
      val name = field.asTerm.name
      val key = name.decodedName.toString
      val returnType = tpe.decl(name).typeSignature

      (q"$key -> t.$name", q"map($key).asInstanceOf[$returnType]")
    }.unzip
    

    // Finally - the actual code generation for the toMap and 
    // fromMap methods
    q"""
      new Mappable[$tpe] {
        def toMap(t: $tpe): Map[String, Any] = Map(..$toMapParams)
        def fromMap(map: Map[String, Any]): $tpe = $companion(..$fromMapParams)
      }
    """
  }
}
