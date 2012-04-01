package ss2js.plugin

import java.io.FileWriter
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.Global
import scala.tools.nsc.Phase
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

/**
 * Helper methods for tree handling, and basic setup.
 */
abstract class BaseComponent(val global: Global, val phaseName: String,
    runAfter: String, verbose: Boolean)
    extends PluginComponent {

    import global._

    val runsAfter = List(runAfter)
    override val runsRightAfter = Some(runAfter)

    def doTree(tree: Tree): String  // abstract

    def writeScript(script: String, sourcename: String) {
        val outfile = global.settings.outdir.value +
        	java.io.File.separator + sourcename + ".js"
        val os = new FileWriter(outfile)
        os.write(script)
        os.close()          
    }

    protected var packageName: String = "need to reset each time"

    def newPhase(prev: Phase) = new StdPhase(prev) {
      def apply(unit: CompilationUnit) {
        reset()
        trace(unit.body)
        val script = doTree(unit.body)
        trace(script)
        writeScript(script, unit.source.toString)
      }
    }

    private def reset() {
      indentSize = 0
      packageName = ""
      NameScope.reset()
    }

    protected object NameScope {
      private val stack = Stack[(Symbol, HashSet[String])]()
      def reset() {
        stack.clear();
        push(NoSymbol)
      }
      def push(s: Symbol) = {
        stack.push((s, new HashSet[String]))
      }
      def pop() {
        stack.pop()
      }
      def add(t: SymTree) = {
        val name = doName(t.symbol.name)
        if(isReserved(name))
          error(t, "reserved word")
        if(t.symbol.name.isOperatorName)
          error(t, "operator overloading not supported")
        if(!stack.top._2.add(name))
          error(t, "duplicate item name not allowed in JS scope: " + name)
        name
      }
      def addPush(t: SymTree) = {
        val ret = add(t)
        push(t.symbol)
        ret 
      }
      def symbol = stack.top._1
    }

    /** name to string */
    protected def doName(n: Name): String = n.decode.trim

    protected def treeInfo(tree: Tree) = {
      "cls: " + tree.getClass.getSimpleName +
      "   tpe: " + tree.tpe +
      (if(tree.hasSymbol)
      "   symbol: " + tree.symbol +
      "   sym tpe: " + tree.symbol.tpe +
//      "   info: " + tree.symbol.info +
        (if(tree.symbol != NoSymbol && tree.symbol.owner != NoSymbol)
        "   sym owner: " + tree.symbol.owner else "")
      else "")
    }

    // output formatting
    private val indentTab = 4
    private var indentSize = 0
    private var indentStr = " "

    protected def indent() = { indentSize += indentTab; " /* in */ "; "" }
    protected def outdent() = { indentSize -= indentTab; " /* ot */ "; "" }
    protected def nl() = {
      while(indentStr.length < indentSize)
        indentStr += indentStr;
      "\n" + indentStr.substring(0, indentSize)
    }

    protected def trace(msg: Any) = if(verbose) println(msg)

    // error reporting
    protected def error(t: Tree, msg: String) = reporter.error(
        t.pos, phaseName + ": " + msg + ": " + t.getClass.getSimpleName)
    protected def warning(t: Tree, msg: String) = reporter.warning(
        t.pos, phaseName + ": " + msg + ": " + t.getClass.getSimpleName)

    // see https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
    private val jsReservedWords = Set(
      "break", "case", "catch", "continue", "debugger", "default", "delete",
      "do", "else", "finally", "for", "function", "if", "in", "instanceof",
      "new", "return", "switch", "this", "throw", "try", "typeof", "var",
      "void", "while", "with",
      "class", "const", "enum", "export", "extends", "import", "super",
      "implements", "interface", "let", "package", "private", "protected",
      "public", "static", "yield",
      "null", "true", "false", "undefined",
      "prototype", "constructor"
      )

    val OUTER = "ss2js_outer"
    val INHERITS = "ss2js_inherits"

    def isReserved(word: String) =
      jsReservedWords.contains(word) || word.startsWith("ss2js_")

    def isUnit(t: Tree) = t.tpe.toString.endsWith("Unit")

    def isUnitish(t: Tree) = {
      val tname = t.tpe.toString;
      tname.endsWith("Unit") || tname.endsWith("Nothing")
    }

    def hasMain(tree: Tree) = {
      tree.symbol.hasModuleFlag && tree.isInstanceOf[ImplDef] &&
      tree.asInstanceOf[ImplDef].impl.body.exists { tree =>
        tree.hasSymbol &&
        tree.symbol.decodedName == "main" &&
        tree.symbol.isSourceMethod &&
        tree.symbol.isPublic
      }
    }

}
