package ss2js.plugin

import java.io.PrintWriter
import scala.tools.nsc.Global

/**
 * Print JavaScript to given output.
 */
class TreePrint[G <: Global](val global: G)(out: PrintWriter) extends Utility {

  import global._

  def print(unit: CompilationUnit) = myprint.print(unit)

  val myprint = new TreePrinter(out) {
    private val scope = new NameScope

    override def print(unit: CompilationUnit) {
      var body = unit.body
      val trans = List(
          new TrimTrans[global.type](global),
          new MangleTrans[global.type](global),
          new ExprTrans[global.type](global),
          new FlattenTrans[global.type](global)
          )
      trans.foreach { t =>
        if(!reporter.hasErrors) {
          trace("------------ " + t);
          body = t.transform(body)
        }
      }
      if(!reporter.hasErrors) {
        printHeader()
        print(body)
        println()
        flush()
      }
    }

    override def printRaw(tree: Tree) = tree match {

      case p: PackageDef => doPackage(p)
      case c: ClassDef => doClass(c)
      case m: ModuleDef => doClass(m)
      case d: DefDef => scope.scope(d)(doDef(d))
      case f: Function => scope.scope(f)(doFunc(f))
      case v: ValDef => doVal(v)
      case i: If => doIf(i)
      case l: LabelDef => doLabel(l)
      case m: Match => doMatch(m)
      case t: Try => doTry(t)

      // TODO: Block not doing braces in all places
      case Block(List(stat), Literal(Constant(()))) => print(stat)
      case Block(stat, Literal(Constant(()))) => print(stat)
      case Block(List(), expr) => print(expr)
      case Block(stats, expr) => print(stats :+ expr)

      // TODO: change way doing native javascript
      case Apply(Select(_, name), List(Literal(Constant(jscript: String))))
      if(doName(name) == "jsni") => // native javascript method
        print(jscript)

      case Apply(fun @ Select(qual, name), vargs)  // operator
      if(name.isOperatorName && vargs.length == 1 && !fun.symbol.isConstructor) =>
        print(fun); print(" "); print(vargs(0))

      case Apply(fun, vargs) =>  // function call
        print(fun); printRow(vargs, "(", ", ", ")")

      case Select(qual, name) if(qual.isInstanceOf[New]) =>
        print(qual)

      case s @ Select(qual, name) if(name.decode == "apply" && s.symbol.isDeferred) =>
        print(qual)

      case Select(qual, name) if(name == nme.CONSTRUCTOR) =>
        print(qual); print(".constructor")

      case Select(qual, name) if(name.startsWith("unary_")) =>
        print(name.decode.drop(6)); print(qual)

      case Select(qual, name) if(name.endsWith("_$eq")) =>
        print(qual); print("."); print(name.decode.dropRight(2)); print(" =")

      case Select(qual, name) if(name.isOperatorName) =>
        print(qual); print(" "); print(name.decode)

      case Select(qual, name) =>
        print(qual); print("."); print(name)

      case TypeApply(fun, args) =>
        print(fun)

      case Assign(lhs, rhs) =>
        print(lhs); print(" = "); print(rhs)

      case t: This => doThis(t)

      // TODO: these probably need to descend - redo New and Super
      case New(tpt) =>
        print("new "); print(tpt.symbol.name) // + doTree(tpt) - TODO: handle other package types

      case s @ Super(qual, mix) =>
        print(s.symbol.superClass.name); print(".prototype")

      case Throw(expr) =>
        print("throw "); print(expr)

      case Return(expr) =>
        print("return "); print(expr)

      case Ident(name) => print(name)
      case Literal(Constant(())) => print("undefined")
//      case Literal(Constant(value: String)) => print("\"" + value + "\"")
      case l: Literal => print(l.toString)
      case t: TypeDef => // ignore
      case t: Tree if(t.isEmpty) => print(" /* empty */ ")

      case _ =>
        error(tree, "not handled: " + tree.getClass().getSimpleName())
        super.printRaw(tree)
    }

    private def printHeader() {
      print("function " + INHERITS + "(child, parent, traits) {"); indent; println()
        print("function F() {"); indent; println()
        print("this.constructor = child"); println()
        print("this.ss2js_traits = traits"); println()
        print("for(var i = traits.length - 1; i >= 0; --i) {"); indent; println()
        print("var p = traits[i].prototype"); println()
        print("for(var f in p) if(p.hasOwnProperty(f)) this[f] = p[f]")
        undent; println(); print("}")
        undent; println(); print("}"); println()
        print("F.prototype = parent.prototype"); println()
        print("child.prototype = new F()")
      undent; println(); print("}")
      println()
      print("function ScalaObject() {}  // placeholder"); println()
    }

    /** package */
    private def doPackage(p: PackageDef) {
      val empty = p.name.startsWith("<empty>")
      val base = p.symbol.owner.name.startsWith("<root>")
      val name = p.name.toString
      if(!empty) {
        if(base)
          print("var " + name); println()
        print("(function(ss2js_package) {  // package " + name); indent; println()
        // TODO: fix generation of octal for \n in print
        // print("\"use strict\";"); println()
      }

      p.stats.foreach(print); println()

      // TODO: revisit main
      p.stats.filter(hasMain).map { i =>
        print(i.symbol.name); print(".main()"); println();
      }
      if(!empty) {
        val name2 = (if(!base) p.symbol.owner.name + "." else "") + name
        undent; println(); print("})(" + name2 + " || (" + name2 + " = {}))")
      }
    }

    /** class, object or trait */
    private def doClass(c: ImplDef) {
      val name = scope.addPush(c)
      print("// " + c.keyword + " " + name + " extends " + tpeName(c.symbol.superClass)); println()
      val body = c.impl.body
      val (defs, others) = body.partition(t =>
        t.isInstanceOf[DefDef] ||(t.hasSymbol && t.symbol.isSynthetic))
      val (ctors, methods) = defs.partition(_.symbol.isConstructor)

      // make ctor function
      println()
      if(ctors.isEmpty) {
        print("function " + name + "() {}  // placeholder for trait")
      }
      else {
        print("function " + name)
        val ctor = ctors.head.asInstanceOf[DefDef]
        val args = ctor.vparamss
        printValueParams(args.flatten)
        // TODO: call trait init methods
        var trees = ctor.rhs :: others
        if(!c.symbol.isTrait) {
          val argslen = ctor.vparamss.flatten.length
          trees = Ident("if(arguments[" + argslen + "]) " +
            "this." + OUTER + " = arguments[" + argslen + "]") :: trees
        }
        print(trees)
        println()
      }

      print("ss2js_package." + name + " = " + name); println()

      // setup inheritence
      if(!c.symbol.isTrait) {
        val parent = tpeName(c.symbol.superClass)
        val traits = c.symbol.mixinClasses.
            map(i => tpeName(i)).mkString("[", ", ", "]")
        print(INHERITS + "(" + name + ", " + parent + ", " + traits + ")")
        println()
      }

      // add methods
      printSeq(methods)(print)(println)

      // create as object if required
      if(c.symbol.hasModuleFlag) {
        // TODO: recheck
        println()
        if(c.symbol.owner.isClass && !c.symbol.owner.hasPackageFlag)
          print("this.")
        else
          print("var ")
        print(name + " = new " + name + "(this)  // object")
      }

      scope.pop()
    }

    /** function def */
    private def doDef(d: DefDef) {
        print("// " + d.keyword + " " + d.name); println()
        if(!d.symbol.owner.isClass) { print("function "); print(d.name) }
        else {
          print(d.symbol.owner.name); print(".prototype.")
          print(d.name); print(" = function")
        }
        printValueParams(d.vparamss.flatten)
        printBlock(d.rhs)
    }

    /** function */
    private def doFunc(f: Function) {
        print("function")
        printValueParams(f.vparams)
        printBlock(f.body)
    }

    /** val */
    private def doVal(v: ValDef) {
        print("// " + v.keyword + " " + v.name); println()
        if(!v.symbol.owner.isClass) print("var ")
        else if(v.symbol.isStatic) { print(v.symbol.owner.name); print(".prototype.") }
        else print("this.")
        print(v.name)
        if(v.symbol.isParamAccessor) { print(" = "); print(v.name) }
        else if(!v.rhs.isEmpty) { print(" = "); print(v.rhs) }
    }

    /** if */
    private def doIf(i: If) {
        print("if("); print(i.cond); print(") "); print(i.thenp)
        if (!isUnit(i.elsep)) {
          println(); print("else "); print(i.elsep)
        }
    }

    /** do while */
    private def doLabel(l: LabelDef) = l.rhs match {
      case Block(block, If(cond, _, _)) if l.name.startsWith(nme.DO_WHILE_PREFIX) =>
        print("do "); print(block); println()
        print("while("); print(cond); print(")")
      case If(cond, Block(block, _), _) if l.name.startsWith(nme.WHILE_PREFIX) =>
        print("while("); print(cond); print(") ")
        print(block)
      case _ => error(l, "unrecognized LabelDef")
    }

    /** basic match as switch */
    private def doMatch(m : Match) {

      def patternMatch(pat: Tree): Unit = pat match {
        case Alternative(list) => // multiple cases with same action
          list.foreach(patternMatch)
        case Literal(_) => // general case
          print("case "); print(pat); print(": ")
        case Ident(nme.WILDCARD) => // default case
          print("default: ")
        case _ =>
          error(pat, "pattern not supported, too complex")
      }

      print("switch("); print(m.selector); print(") {"); indent(); println()

      m.cases.foreach { c =>
        patternMatch(c.pat); print(c.body); println(); print("break"); println()
      }

      undent(); println(); print("}")
    }

    /** try catch finally */
    private def doTry(t: Try) = {

      def catchName(pat: Tree) = pat match {
        case Ident(_) => "ss2js_error"
        case Bind(name, Ident(_)) => doName(name)
        case _ => error(pat, "only simple names supported")
      }

      print("try"); printBlock(t.block); println()

      t.catches match {
        case List() =>
        case List(CaseDef(pat, guard, body)) =>
          print("catch(" + catchName(pat) + ")"); printBlock(body); println()
        case _ => error(t.catches(1), "only one catch allowed")
      }

      if(t.catches.isEmpty || !t.finalizer.isEmpty) {
        print("finally"); printBlock(t.finalizer)
      }
    }

    /** find correct this at different levels in heirarchy */
    private def doThis(t: This): Unit = {

      val chain = scope.symbol.enclClassChain
      def doError(msg: String) = { error(t, msg + ": " + t.symbol + "  " + chain); "" }

      def findThis(chain: List[Symbol]): String = {
        if(chain.isEmpty) doError("did not find outer")
        else if(chain.head == NoSymbol) doError("NoSymbol in chain")
        else if(t.symbol == chain.head) "this"
        else findThis(chain.tail) + "." + OUTER
      }

      if(t.symbol.hasModuleFlag)
        if(t.qual.nonEmpty) print(t.qual) else print("this")
      else
        print(findThis(chain))
    }

    def print(trees: List[Tree]) { printColumn(trees, " {", "", "}") }
    override def printBlock(tree: Tree) { printColumn(List(tree), " {", "", "}") }
    override def print(name: Name) { print(name.decode.trim) }
    override def printParam(tree: Tree) { print(tree.symbol.name) }

    override def printFlags(flags: Long, privateWithin: String) {}
    override def printTypeParams(ts: List[TypeDef]) {}
    override def printAnnotations(tree: Tree) {}
  }

}
