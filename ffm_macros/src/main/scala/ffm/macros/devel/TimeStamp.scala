package ffm.macros.devel

import scala.language.experimental.macros 
import scala.reflect.macros.blackbox.Context

/**
 * Provides a simple macro to generate a compile-time timestamp.
 * 
 * Adapted from [[https://gist.github.com/fancellu/1020d04e7972cb78d328]]
 * 
 * Example:
 * {{{
 * val header = "Program compiled: " + TimeStamp.get()
 * }}}
 */
object Timestamp {
  def get(): String = macro impl

  def impl(c: Context)(): c.Tree = {
    import c.universe._

    val now = new java.util.Date().toString

    q"$now"
  }
}
