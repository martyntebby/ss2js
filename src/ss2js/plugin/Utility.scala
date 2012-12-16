package ss2js.plugin

import scala.tools.nsc.Global
import scala.collection.mutable.Stack
import scala.collection.mutable.HashSet

/**
 * Various common methods.
 */
abstract class Utility {

  val global: Global
  val verbose = true;	// TODO: better way to set
  import global._

  def error(tree: Tree, msg: String) = reporter.error(tree.pos, "ss2js: " + msg)
  def warning(tree: Tree, msg: String) = reporter.warning(tree.pos, "ss2js: " + msg)

  def trace(msg: Any) = if(verbose) println(msg)

  def tpeName(sym: Symbol): String = sym.tpe.toString match {
    case "java.lang.Object" => "Object"
    case str => str
  }

  def treeInfo(tree: Tree): String = {
    if(tree == null) return "null"
    "cls: " + tree.getClass.getSimpleName +
    "   tpe: " + tree.tpe +
    (if(tree.hasSymbol)
      "   symbol: " + tree.symbol +
      "   sym tpe: " + tree.symbol.tpe +
      (if(tree.symbol != NoSymbol && tree.symbol.owner != NoSymbol)
        "   sym owner: " + tree.symbol.owner else "")
    else "") +
    (if(tree.isDef) "  name: " + tree.asInstanceOf[DefTree].name else "")
  }

  /** Is this an object with a main method */
  def hasMain(tree: Tree) = {
    tree.symbol.hasModuleFlag && tree.isInstanceOf[ImplDef] &&
    tree.asInstanceOf[ImplDef].impl.body.exists { tree =>
      tree.hasSymbol &&
      tree.symbol.decodedName == "main" &&
      tree.symbol.isSourceMethod &&
      tree.symbol.isPublic
    }
  }

  def isUnit(tree: Tree) = tree match {
    case Literal(Constant(())) => true
    case _ => false
  }

  def isUnitish(tree: Tree) = {
    // TODO: properly
      val strtype = tree.tpe.toString()
      strtype.endsWith("Unit") || strtype.endsWith("Nothing")
  }

    /** name to string */
    def doName(n: Name): String = n.decode.trim

    /**
     * Keep track of names in current scope and error duplicates.
     */
    class NameScope {
      private val stack = Stack[(Symbol, HashSet[String])]()
      push(NoSymbol)

      def symbol = stack.top._1
      def push(s: Symbol) = {
        stack.push((s, new HashSet[String]))
      }
      def pop() {
        stack.pop()
      }
      def add(t: SymTree) = {
        val name = doName(t.symbol.name)
        if(!t.isInstanceOf[Function] && !t.symbol.isConstructor) {
          if(!t.symbol.isConstructor && isReserved(name))
            error(t, "reserved word: " + name)
          if(t.symbol.name.isOperatorName)
            error(t, "operator overloading not supported: " + name)
          if(!stack.top._2.add(name))
            error(t, "duplicate item name not allowed in JS scope: " + name)
        }
        name
      }
      def contains(t: SymTree) = stack.top._2.contains(doName(t.symbol.name))
      def addPush(t: SymTree) = {
        val ret = add(t)
        push(t.symbol)
        ret
      }
      def scope[T](t: SymTree)(func: => T) = {
        val name = add(t)
        push(t.symbol)
        val ret = func
        pop()
        ret
      }
    }

    // see https://developer.mozilla.org/en/JavaScript/Reference/Reserved_Words
    val jsReservedWords = Set(
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

    def isReserved(word: String) =
      jsReservedWords.contains(word) || word.startsWith("ss2js_")

    val OUTER = "ss2js_outer"
    val INHERITS = "ss2js_inherits"

}
