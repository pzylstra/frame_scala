package ffm.io

import ExpressionSyntax._

/**
 * A FallbackProvider is used to store default values for parameters
 * required by factory classes which construct model objects from
 * input definitions (e.g. SiteFactory).
 */
trait FallbackProvider {
  def parent: FallbackProvider
  def fallbacks: Map[String, ValueAssignment]
}

object FallbackProvider {

  /**
   * Creates a top-level FallbackProvider from a lookup table of 
   * String -> `A` elements where `A` is a supported Expression
   * value type.
   */
  def apply[A: ValueLike](fallbacks: Map[String, A]): FallbackProvider =
    FallbackProvider(valueAssignmentMap(fallbacks))

  /**
   * Creates a top-level FallbackProvider from a lookup table of
   * String -> ValueAssignment elements.
   */
  def apply(fallbacks: Map[String, ValueAssignment]): FallbackProvider =
    new ChildFallbackProvider(Default, fallbacks)

  /**
   * Creates a child FallbackProvider from a lookup table of
   * String -> `A` elements where `A` is a supported Expression
   * value type.
   * 
   * This provider's elements take precedence over those of the parent.
   */
  def apply[A: ValueLike](parent: FallbackProvider, fallbacks: Map[String, A]): FallbackProvider =
    new ChildFallbackProvider(parent, valueAssignmentMap(fallbacks))

  /**
   * Creates a child FallbackProvider from a lookup table of
   * String -> ValueAssignment elements.
   * 
   * This provider's elements take precedence over those of the parent.
   */
  def apply(parent: FallbackProvider, fallbacks: Map[String, ValueAssignment]): FallbackProvider =
    new ChildFallbackProvider(parent, fallbacks)

  /**
   * Default top-level provider with an empty lookup table.
   */
  object Default extends FallbackProvider {
    def parent = this
    def fallbacks = Map.empty[String, ValueAssignment]
  }

  private class ChildFallbackProvider(val parent: FallbackProvider, localFallbacks: Map[String, ValueAssignment]) extends FallbackProvider {
    // local fallbacks take precedence over those of the parent
    def fallbacks: Map[String, ValueAssignment] = parent.fallbacks ++ localFallbacks
  }

  private def valueAssignmentMap[A: ValueLike](m: Map[String, A]): Map[String, ValueAssignment] =
    m map {
      case (k, v) => v match {
        case i: Int => (k, ValueAssignment(Text(k), IntegerLiteral(i)))
        case d: Double => (k, ValueAssignment(Text(k), DoubleLiteral(d)))
        case s: String => (k, ValueAssignment(Text(k), Text(s)))
      }
    }

}