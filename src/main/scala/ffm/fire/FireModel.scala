package ffm.fire

/**
 * Base trait for vegetation fire models.
 */
trait FireModel {
  def run(): FireModelRunResult
}
