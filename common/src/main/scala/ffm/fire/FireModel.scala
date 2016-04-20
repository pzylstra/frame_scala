package ffm.fire

import ffm.forest.Site

/**
 * Base trait for classes implementing vegetation fire models.
 */
trait FireModel {
  def run(site: Site, includeCanopy: Boolean): FireModelRunResult
}
