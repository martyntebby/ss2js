package ss2js.plugin

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.util.trace
import scala.tools.nsc.Phase

/**
 * Scala compiler plugin that generates JavaScript.
 * It handles a subset of Scala that easily maps to JavaScript,
 * and generates compilation errors for constructs it does not understand.
 * 
 * Limitations:
 *	Single compilation unit and package
 *	Does not use the standard (Predef) imports, use import ss2js.global._
 *	Single constructor per class
 *	Cannot have methods or variables with the same name
 *	Currently unsupported:
 *		lazy, operator overloading, case classes
 *		companion objects
 */
class CompilerPlugin(val global: Global, runAfter: String, verbose: Boolean)
    extends Plugin {

  def this(global: Global) = this(global, "typer", false)

  val name = "ss2js"
  val description = "converts a subset of Scala to JavaScript"
  val components = List(new PlugComponent(global, name, runAfter, verbose))
}
