package ss2js.plugin

import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.Global

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
 *	Ignores concrete trait methods
 *	Currently unsupported:
 *		match, lazy, operator overloading, case classes
 *		companion objects, apply method
 *
 * @author Martyn Tebby
 */
class CompilerPlugin(val global: Global, runAfter: String, verbose: Boolean)
    extends Plugin {

  def this(global: Global) = this(global, "typer", false)

  val name = "ss2js"
  val description = "converts a subset of Scala to JavaScript"
  val components = List(new TreeComponent(global, name, runAfter, verbose))
/*
  private var verbose = false

  override val optionsHelp: Option[String] =
    Some("  -P:" + name + ":verbose        internal tracing")

  override def processOptions(options: List[String], error: String => Unit) {
    options.foreach { opt =>
      if(opt == "verbose") verbose = true
    }
  }
*/
}
