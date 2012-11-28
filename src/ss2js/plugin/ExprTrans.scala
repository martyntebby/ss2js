package ss2js.plugin

import scala.tools.nsc.Global
  
/**
 * Convert scala expressions to JS statements.
 * Push val, assign, return and throw down to JS expression.
 * E.g.  val a = { 1; 2 }  ->  { 1; val a = 2 }
 * 
 * TODO: should expand to all binary operators     
 */
class ExprTrans[G <: Global](val global: G) extends TreeTrans {

  import global._

  val transformer = new Transformer {

    override def transform(tree: Tree) = transform(tree, t => t)

    def transform(tree: Tree, prefix: Tree => Tree): Tree = {

      trace(treeInfo(tree))
      tree match {

        // Assignments - create prefix

        case ValDef(mods, name, tpt, rhs)
            if(!tree.symbol.isParameter && !tree.symbol.isParamAccessor) =>
          transform(rhs, treeCopy.ValDef(tree, transformModifiers(mods), name, transform(tpt), _))

        case Assign(lhs, rhs) =>
          transform(rhs, treeCopy.Assign(tree, transform(lhs), _))

        case Return(expr) =>
          transform(expr, treeCopy.Return(tree, _))

        case Throw(expr) =>
          transform(expr, treeCopy.Throw(tree, _))

        case Apply(fun @ Select(_, name), List(arg)) if(name.endsWith("_$eq")) =>
          transform(arg, arg2 => treeCopy.Apply(tree, transform(fun), List(arg2)))

        // JS Statements - pass through prefix

        case Block(stats, expr) =>
          treeCopy.Block(tree, transformStats(stats, null), transform(expr, prefix))

        case If(cond, thenp, elsep) =>
          treeCopy.If(tree, transform(cond), transform(thenp, prefix), transform(elsep, prefix))

        case Match(selector, cases) =>
          treeCopy.Match(tree, transform(selector), transformCaseDefs(cases, prefix))

        case CaseDef(pat, guard, body) =>
          treeCopy.CaseDef(tree, transform(pat), transform(guard), transform(body, prefix))

        case Try(block, catches, finalizer) =>
          treeCopy.Try(tree, transform(block, prefix), transformCaseDefs(catches, prefix), transform(finalizer))

        // JS Expressions - apply prefix

        case Function(vparams, body) =>
          prefix(treeCopy.Function(tree, transformValDefs(vparams), transform(body)))

        case Apply(fun, args) =>
          prefix(treeCopy.Apply(tree, transform(fun), transformTrees(args)))

        case Select(qualifier, selector) =>
          prefix(treeCopy.Select(tree, transform(qualifier), selector))

        case Ident(name) =>
          prefix(treeCopy.Ident(tree, name))

        case Literal(value) =>
          prefix(treeCopy.Literal(tree, value))

        case _ =>
          val pretree = prefix(tree)
          if(pretree != tree)
            error(pretree, "cannot convert operation, try using temp variable: " + tree.symbol)
          super.transform(tree)
      }
    }

    def transformCaseDefs(trees: List[CaseDef], prefix: Tree => Tree) =
      trees mapConserve (tree => transform(tree, prefix).asInstanceOf[CaseDef])
  }

}
