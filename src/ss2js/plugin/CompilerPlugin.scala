package ss2js.plugin

import java.io.PrintWriter
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
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


  class PlugComponent(
    val global: Global, val phaseName: String,
    runAfter: String, verbose: Boolean)
    extends PluginComponent {

  import global._

  val runsAfter = List(runAfter)
  override val runsRightAfter = Some(runAfter)

  def newPhase(prev: Phase) = new StdPhase(prev) {
    def apply(unit: CompilationUnit) {
      val outfile = global.settings.outdir.value +
          java.io.File.separator + unit.source + ".js"
      val out = new PrintWriter(outfile)
      new TreePrint[global.type](global)(out).print(unit)
      out.close()
    }
  }
}

}
