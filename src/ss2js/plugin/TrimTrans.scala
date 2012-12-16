package ss2js.plugin

import scala.tools.nsc.Global

/**
 * Remove accessor methods, abstract methods, unused trees, aliases.
 * Signal error for unsupported constructs.
 * Detect duplicate names.
 */
class TrimTrans[G <: Global](val global: G) extends TreeTrans {

  import global._

  private val scope = new NameScope

  val transformer = new Transformer {
    override def transform(tree: Tree): Tree = {
    trace(treeInfo(tree))
    tree match {

      case PackageDef(Select(qual, name), stats) =>
        val p1 = PackageDef(Ident(name), stats).setSymbol(tree.symbol)
        val p2 = PackageDef(qual.asInstanceOf[RefTree], List(p1))
        p2.setSymbol(tree.symbol.owner)
        transform(p2)

      case p: PackageDef =>
        scope.scope(p) { super.transform(tree) }

      case _: Import =>
        EmptyTree  // remove import

      case i: ImplDef =>
        if(i.mods.isCase)
          error(tree, "case class not supported")
        if(i.symbol.isTrait && !i.symbol.isInterface) {
          error(i, "non interface trait not supported")
        }
        scope.scope(i) { super.transform(tree) }

      case v @ ValDef(mods, name, tpt, rhs) =>
        if(v.symbol.alias != NoSymbol) {  // remove alias
          // TODO: maybe check for paramaccessor
          if(v.symbol.alias.decodedName.trim != v.symbol.decodedName.trim)
            error(tree, "use same name for aliased variable " + v.symbol.alias.name)
          return EmptyTree
        }
        if(v.mods.isLazy)
          error(tree, "lazy not supported")
        scope.add(v)
        super.transform(tree)

      case d @ DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        val symbol = tree.symbol
        if(mods.isDeferred)  // remove abstract method
          return EmptyTree
        if(symbol.hasAccessorFlag) {  // remove accessor
          if(symbol.isOverride)
            error(tree, "override val not supported")
          return EmptyTree
        }
        /*
        // Maybe bring back later
        if(symbol.isSynthetic && symbol.hasDefaultFlag) {
          // convert default method parameter to val
          rhs match {
            case _: Literal => // TODO: define const properly - use flag
            case _ => error(rhs, "constant defaults only")
          }
          if(vparamss.nonEmpty)
            error(rhs, "default param with multiple param lists nor supported")
          return treeCopy.ValDef(tree, transformModifiers(mods), name, transform(tpt), transform(rhs))
        }
        */
        if(symbol.isConstructor && !symbol.isPrimaryConstructor)
          error(tree, "multiple constructors not supported")
        if(name.endsWith("_$eq"))
          error(tree, "overriding = not supported")
        if(symbol.isVarargsMethod)
          error(tree, "varargs not supported")
        scope.scope(d) { super.transform(tree) }

      case f: Function =>
        scope.scope(f) { super.transform(tree) }

      case c: CaseDef =>
        if(!c.guard.isEmpty)
          error(c.guard, "guard not supported")
        super.transform(tree)

      case t: Try =>
        if(t.catches.length > 1)
          error(t.catches(1), "multiple catches not supported")
        super.transform(tree)

      case _ =>
        super.transform(tree)
    }
    }

    // check for varargs
    override def transformValDef(tree: ValDef): ValDef = {
      if(global.treeInfo.isRepeatedParamType(tree.tpt))
        error(tree.tpt, "varargs not supported")
      if (tree.isEmpty) tree else transform(tree).asInstanceOf[ValDef]
    }

    // check default case is the last
    override def transformCaseDefs(trees: List[CaseDef]): List[CaseDef] = {
      if(trees.nonEmpty)
        trees.last.pat match {
          case Ident(_) =>
          case Bind(_, Ident(_)) =>
          case _ => warning(trees.last.pat, "last case is not default")
        }
      super.transformCaseDefs(trees)
    }

  }

}
