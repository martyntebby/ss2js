package ss2js.plugin

import scala.tools.nsc.Global

/**
 * Main tree handling.
 * Returns JavaScript representation.
 * see end of scala.reflect.generic.Trees
 */
class TreeComponent(override val global: Global, phaseName: String,
    runAfter: String, verbose: Boolean)
    extends BaseComponent(global, phaseName, runAfter, verbose) {

    import global._

    /** primary method */
    override def doTree(tree: Tree): String = {
      trace("// " + treeInfo(tree))
      tree match {
          case p: PackageDef => doPackage(p)
          case i: Import => ""
          case c: ClassDef => doClass(c)
          case m: ModuleDef => doClass(m)
          case d: DefDef => doDef(d)
          case b: Block => doBlock(b, false)
          case f: Function => doFunction(f)

          case v: ValDef => doVal(v)
          case t: TypeDef => ""
          case i: If => doIf(i)
          case l: LabelDef => doLabel(l)
          case m: Match => doMatch(m)
          case t: Try => doTry(t)

          case t: TypeApply => doTree(t.fun)
          case a: Apply => doApply(a)
          case s: Select => doSelect(s)
          case Assign(lhs, rhs) => doTree(lhs) + " = " + doTree(rhs)

          // TODO: these probably need to descend
          case New(tpt) => "new " + tpt.symbol.name // + doTree(tpt) - TODO: handle other package types
          case t: This => doThis(t)
          case s: Super => s.symbol.superClass.name + ".prototype"

          case Throw(expr) => "throw " + doTree(expr)
          case Return(expr) => "return " + doTree(expr)
          case Ident(name) => doName(name)
          case Literal(Constant(())) => "undefined"
          case l: Literal => l.toString
          case t: TypeTree => ""

          case t: Tree if(t.isEmpty) => ""
          case _ => error(tree, "not handled"); ""
        }
    }

    /** do list of trees */
    private def doTrees(list: List[Tree]) =
      list.map(doTree).filter(_ != "").mkString(nl)

    /** outer (only) package, adds ss2js methods */
    private def doPackage(p: PackageDef) = {
      if(packageName != "")
        error(p, "multiple packages not supported")
      packageName = NameScope.addPush(p)

      val str = "(function() {" + indent + nl +
      "\"use strict\";" + nl +
      // ss2js methods
      "function " + INHERITS + "(child, parent, traits) {" + indent + nl +
        "function F() {}" + nl +
        "F.prototype = parent.prototype;" + nl +
        "child.prototype = new F();" + nl +
        "child.prototype.constructor = child;" + nl +
//        "child.prototype.ss2js_traits = traits" + nl +
        "for(var i = traits.length - 1; i >= 0; --i) {" + indent + nl +
        "var p = traits[i].prototype" + nl +
        "for(var f in p) if(p.hasOwnProperty(f)) child.prototype[f] = p[f]" +
        outdent + nl + "}" +
//        "child.prototype.prototype = parent.prototype;" +
//        "child.prototype.__proto__ = parent.prototype;" +
      outdent + nl + "}" + nl +
      "function ScalaObject() {}  // dummy" + nl + nl +
      doTrees(p.stats) + nl + // main body
      p.stats.filter(hasMain).map(_.symbol.name + ".main();").mkString(nl) +
      outdent + nl + "}());" + nl

      NameScope.pop()
      str
    }

    /** class, trait and object/module */
    private def doClass(c: ImplDef): String = {
      val name = NameScope.addPush(c)
      val body = c.impl.body
      if(c.mods.isCase)
        error(c, "case class not supported")
      if(c.symbol.isTrait) {
        if(body.exists(_.symbol.isConstructor))
          warning(c, "concrete trait incomplete")
      }

      // collect class variables and methods
      val allvals = body.filter { case v: ValDef =>
        			true; case _ => false } .asInstanceOf[List[ValDef]]
      val alldefs = body.filter { case d: DefDef =>
        			true; case _ => false } .asInstanceOf[List[DefDef]]
      val (aliases, vals) = allvals.partition(_.symbol.alias != NoSymbol)
      val (accessors, defs) = alldefs.partition(_.mods.hasAccessorFlag)
      val (ctors, methods) = defs.partition(_.symbol.isConstructor)

      if(ctors.length > 1)
        error(ctors(1), "multiple constructors not supported")
      accessors.filter(_.symbol.isOverride).foreach {
          error(_, "override val not supported")
      }
      aliases.foreach { a =>
        if(a.symbol.alias.name != a.symbol.name)
          error(a, "use same name for aliased variable " + a.symbol.alias.name)
      }

      var str = ""

      // make ctor function
      if(ctors.isEmpty) {
        str = "function " + name + "() {}  // dummy for trait" + nl
      }
      else {
        val ctor = ctors.head
        str = "function " + name + doParams(ctor) + " {" + indent + nl
        if(!c.symbol.isTrait) {
          val argslen = ctor.vparamss.flatten.length
          str += "if(arguments[" + argslen + "]) " +
            "this." + OUTER + " = arguments[" + argslen + "]" + nl
        }
        // TODO: tidy up
        // TODO: call trait init methods
        val lines = doTree(ctor.rhs) ::
            vals.map(doTree(_)) ++
            body.filter(!_.isInstanceOf[ValOrDefDef]).map(doTree)
        str += lines.filter(_ != "").mkString(nl)
        str += outdent + nl + "}" + nl
      }

      // setup inheritence
      if(!c.symbol.isTrait) {
        val parent = c.symbol.superClass.name
        val traits = c.symbol.mixinClasses.
            map(_.decodedName).mkString("[", ", ", "]")
        str += INHERITS + "(" + name + ", " + parent + ", " + traits + ")" + nl
      }

      // add methods
      str += methods.map(doTree(_) + nl).mkString

      // create as object if required
      if(c.symbol.hasModuleFlag) {
        // TODO: recheck
        str += (if(c.symbol.owner.isClass && !c.symbol.owner.hasPackageFlag) "this."
                  // c.symbol.owner.name + ".prototype."
                else "var ")
        str += name + " = new " + name + "(this);  // object"
      }
      NameScope.pop()
      str
    }

    /** method parameters */
    private def doParams(d: DefDef): String = {
      if(d.vparamss.isEmpty)
        error(d, "parameter list required")
      if(d.symbol.isVarargsMethod)
        error(d, "varargs not supported")
      doParams(d.vparamss.flatten)
    }

    /** method parameters */
    private def doParams(params: List[ValDef]) =
      params.map { v =>
        if(v.symbol.isByNameParam)
          error(v, "input types must be specified, even if empty")
        doName(v.name)
      }.mkString("(", ", ", ")")

    /** method definition */
    private def doDef(d: DefDef): String = {
      if(d.symbol.isDeferred) return ""
      val name = NameScope.addPush(d)
      if(name == "apply")
        error(d, "apply not supported")
      // method parameter with default value
      if(d.symbol.isSynthetic && name.contains("$default$")) {
        val valDef = ValDef(d.mods, d.name, d.tpt, d.rhs)
        valDef.symbol = d.symbol.cloneSymbol
        return doTree(valDef)
      }
      // TODO: get rid of last, does the block have the same type as the owner - probably
      val last = if(d.tpt.toString != "Unit") "return " else ""  // TODO: another way
      val rest = doParams(d) + doBlock(d.rhs, true, last)
      NameScope.pop()
      if(!d.symbol.owner.isClass) "function " + name + rest
      else d.symbol.owner.name + ".prototype." + name + " = function " + rest
    }

    /** format a block of code */
    private def doBlock(tree: Tree, braces: Boolean, last: String = ""): String = {
      (if(braces) " {" + indent + nl else "") +
        (tree match {
          case Block(List(stat), Literal(Constant(()))) => // single item and unit return
            doTree(stat)
          case Block(List(), expr) => // just return part
            doBlock(expr, false, last)
          case Block(_, _) if !braces => // add braces to real block
            doBlock(tree, true, last)
          case Block(stats, expr) => // general block case
            doTrees(stats) + nl + doBlock(expr, false, last)
          case Literal(Constant(())) => // unit
            if(last != "") last + doTree(tree) else ""
          case Apply(Select(_, name), List(Literal(Constant(jscript: String))))
          	if(doName(name) == "jsni") => // native javascript
          	  jscript
          case _ =>
            last + doTree(tree)
        }) +
      (if(braces) outdent + nl + "}" else "")
    }

    /** closure */
    private def doFunction(f: Function): String = {
      NameScope.push(f.symbol)
      val last = if(isUnitish(f)) "" else "return "
      val str = "function" + doParams(f.vparams) + doBlock(f.body, true, last)
      NameScope.pop()
      str
    }

    /** val and var */
    private def doVal(v: ValDef): String = {
      val name = NameScope.add(v)
      if(v.mods.isLazy)
        error(v, "lazy not supported")
      val prefix = if(!v.symbol.owner.isClass) "var "
        else if(v.symbol.isSynthetic && name.contains("$default$")) {
          // TODO: generalize or remove
          if(v.rhs.isInstanceOf[This])
            error(v.rhs, "cannot have this as default parameter")
          v.symbol.owner.name + ".prototype."
        }
        else "this."
      if(v.symbol.isParamAccessor) prefix + name + " = " + name
      else if(v.rhs.isEmpty) prefix + name
      else prefix + name + " = " + doTree(v.rhs)
    }

    /** if else, sometimes as conditional */
    private def doIf(i: If) = {
      val unit = isUnit(i)
      val block = i.thenp.isInstanceOf[Block] || i.elsep.isInstanceOf[Block]
      if(!unit && block)
        error(i, "cannot convert multi line if to conditional")
      if(unit || block)
        "if(" + doTree(i.cond) + ") " + doTree(i.thenp) + nl + "else " + doTree(i.elsep)
      else
        doTree(i.cond) + " ? " + doTree(i.thenp) + " : " + doTree(i.elsep)
    }

    /** do while */
    private def doLabel(l: LabelDef) = {
      l.rhs match {
        case Block(block, If(cond, _, _)) if l.name.startsWith("doWhile$") =>
          "do " + doTrees(block) + nl + "while(" + doTree(cond) + ")"
        case If(cond, Block(block, _), _) if l.name.startsWith("while$") =>
          "while(" + doTree(cond) + ") " + doTrees(block)
        case _ => error(l, "unrecognized LabelDef"); ""
      }
    }

    /** basic match as switch */
    private def doMatch(m : Match) = {
      if(!isUnitish(m))
        error(m, "only Unit type allowed for match")

      var haveDefault = false
      val selector = doTree(m.selector)
      val start = "switch(" + selector + ") {" + indent + nl

      def patternMatch(pat: Tree): String = pat match {
        case Alternative(list) => // multiple cases with same action
          list.map(patternMatch).mkString(": ")
        case Literal(_) => // general case
          if(haveDefault)
            error(pat, "after default case")
          "case " + doTree(pat)
        case Ident(_) => // default case
          haveDefault = true; "default"
        case _ =>
          error(pat, "pattern not supported, too complex"); ""
      }

      val cases = m.cases.map { c =>
        if(!isUnitish(c.body))
          error(c.body, "only Unit type allowed in match case")
        patternMatch(c.pat) + ": " + doTree(c.body) + nl + "break;"
      }

      val default = if(haveDefault) ""
        else nl + "default: throw new Error('Match: " + selector + "')" +
              nl + "break;"

      start + cases.mkString(nl) + default + outdent + nl + "}"
    }

    /** try catch finally */
    private def doTry(t: Try): String = {
      if(!isUnitish(t))
        error(t.block, "only Unit type allowed in try")

      def catchesMatch = t.catches match {
        case List() => ""
        case List(CaseDef(pat, guard, body)) =>
          if(!guard.isEmpty) error(guard, "catch guard is not supported")
          "catch(" + catchName(pat) + ")" + doBlock(body, true) + nl
        case _ => error(t.catches(1), "only one catch allowed")
      }

      def catchName(pat: Tree) = pat match {
        case Ident(_) => "ss2js_error"
        case Bind(name, Ident(_)) => doName(name)
        case _ => error(pat, "only simple names supported")
      }

      "try" + doBlock(t.block, true) + nl +
      catchesMatch +
      "finally" + doBlock(t.finalizer, true)
    }

    /** method / operator call and arguments */
    private def doApply(a: Apply) = {
      var call = ""
      def funargs(a: Apply): (Tree, List[Tree]) = a match {
        case Apply(sel @ Select(New(tpt), name), args) => // new
          (sel, args :+ Ident("this"))
        case Apply(sel @ Select(Super(qual, mix), name), args) => // super
          call = ".call"; (sel, qual :: args)
        case Apply(a: Apply, args2) => // multiple argument lists
          val (fun, args1) = funargs(a); (fun, args1 ++ args2)
        case Apply(fun, args) => // general case
          (fun, args)
      }
      val (fun, args) = funargs(a)
      doTree(fun) + call +
      (if(a.symbol.name.isOperatorName && !a.symbol.isConstructor && args.length == 1)
        " " + doTree(args(0))  // binary operator call
      else
        args.map(doTree).mkString("(", ", ", ")"))
    }

    /** method selection */
    private def doSelect(s: Select) = {
      val name = doName(s.name)
      val qual = doTree(s.qualifier)
      // TODO: revisit apply
      if(name == "apply") { /* warning(s, "apply"); */ qual }
      else if(s.qualifier.isInstanceOf[New]) qual
      else if(s.symbol.isConstructor) qual + ".constructor"
      else if(name.startsWith("unary_")) name.drop(6) + qual
      else if(name.endsWith("_=")) qual + "." + name.dropRight(2) + " ="
      else if(s.name.isOperatorName) qual + " " + name
      else qual + "." + name
    }

    /** find correct this at different levels in heirarchy */
    private def doThis(t: This): String = {
      val chain = NameScope.symbol.enclClassChain
      def doError(msg: String) = { error(t, msg + ": " + t.symbol + "  " + chain); "" }
      def findThis(chain: List[Symbol]): String = {
        if(chain.isEmpty) doError("did not find outer")
        else if(chain.head == NoSymbol) doError("NoSymbol in chain")
        else if(t.symbol == chain.head) "this"
        else findThis(chain.tail) + "." + OUTER
      }
      if(t.symbol.hasModuleFlag)
        if(t.qual.nonEmpty) doName(t.qual) else "this"
      else
        findThis(chain)
    }

}
