package ffm.io

/**
 * Holds the details of a parameter used in creating a new model object.
 * 
 * @param key name as used in parameter file
 * @param argName associated object argument name
 * @param optional true if this argument is optional
 */
class FactoryItem(val key: String, val argName: String, val optional: Boolean)

object FactoryItem{
  def item(key: String, argName: String, optional: Boolean = true) = 
    new FactoryItem(key, argName, optional)
}
