package ffm.forest

/**
 * Holds parameters for site context.
 * 
 * Site context consists of properties of a site's surrounds
 * (other than weather) which may influence fire behaviour within 
 * the site. Presently, only fire line length is included.
 * 
 * @param fireLineLength fire line length (metres)
 */
case class SiteContext(fireLineLength: Double)
