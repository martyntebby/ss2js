package ss2js.test

import scala.tools.nsc.CompilerCommand
import scala.tools.nsc.reporters.ConsoleReporter
import scala.tools.nsc.Global
import scala.tools.nsc.Settings
import ss2js.plugin.CompilerPlugin
import java.io.FileReader
import javax.script.ScriptEngineManager

/**
 * Run the plugin, then run generated files in Rhino.
 * If no args given use default set to compile files in sample directory.
 * If one arg is "eval" only run the rhino part. (problems running from ant)
 */
object RunPlugin {

  val runAfter = "typer"
  val outdir = "bin"
  val files = List(
      "src/ss2js/sample/Sample.scala"
,      "src/ss2js/sample/Simple.scala"
      )
  val args = Array(
    "-Yno-imports",
//  "-Xprint:" + runAfter,
//  "-Ybrowse:" + runAfter,
//  "-Yshow-trees",
    "-classpath", "bin",
    "-d", outdir
    ) ++ files

  def main(args: Array[String]) {
    val args2 = if(args.isEmpty) this.args else args
    if(args2(0) != "eval")
      if(!compile(args2.toList))
        return
    val jsfiles = files.map { f =>
      outdir + f.substring(f.lastIndexOf("/")) + ".js" }
    run(jsfiles)
  }

  def compile(args: List[String]) = {
    val settings = new Settings()
    val reporter = new ConsoleReporter(settings)
    val command  = new CompilerCommand(args, settings)
    val compiler = new Global(settings, reporter) {
      val plugin = new CompilerPlugin(this, runAfter, true)
      override def loadRoughPluginsList = plugin :: super.loadRoughPluginsList
    }
    val run = new compiler.Run()
    run.compile(command.files)
    reporter.printSummary()
    !reporter.hasErrors
  }

  // eval javascript files
  def run(files: List[String]) {
    val manager = new ScriptEngineManager()
    val engine = manager.getEngineByExtension("js")
    files.foreach { file =>
      println("run " + file)
      val reader = new FileReader(file)
      engine.eval(reader)
      reader.close()
    }
    println("OK")
  }

}
