package ss2js

/**
 * Standard set of global JavaScript Objects.
 * TODO: define properly with actual methods
 */
object global {

  type Object = java.lang.Object
  type String = java.lang.String
  type Number = scala.Double
  type Double = scala.Double
  type Boolean = scala.Boolean
  type Date = java.util.Date

  type Error = java.lang.Exception

  // non standard stuff

  type Int = scala.Int // non standard

  type Unit = scala.Unit
  type Any = scala.Any
  type AnyRef = scala.AnyRef
  type AnyVal = scala.AnyVal
  type Nothing = scala.Nothing

  /** Allow raw javascript as body of method */
  def jsni(rawJavaScriptBody: String): Nothing = sys.error("jsni snippet should be handled by ss2js") 

  // TODO
  // type Array
  // type RegExp
  // type Function
}
