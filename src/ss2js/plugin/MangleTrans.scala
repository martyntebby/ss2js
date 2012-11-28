package ss2js.plugin

import scala.tools.nsc.Global

/**
 * Change tree structure closer to JavaScript.
 * Add return to method and function.
 * Add class outer vars.
 * Flatten multiple method argument lists and add empty one when none.
 */
class MangleTrans[G <: Global](val global: G) extends TreeTrans {

  import global._

  val transformer = new Transformer {

    override def transform(tree: Tree): Tree = {
    trace(treeInfo(tree))
    tree match {

      case DefDef(mods, name, tparams, vparamss, tpt, rhs) =>
        // add return to method
        super.transform(DefDef(tree.symbol, returnWrap(tree, rhs)))

      case Function(vparams, body) =>
        treeCopy.Function(tree, transformValDefs(vparams),
            transform(returnWrap(tree, body)))  // add return to function

      case Apply(Apply(fun, args1), args2) => // collapse multiple param lists
        transform(treeCopy.Apply(tree, fun, args1 ++ args2))

      case Apply(s @ Select(New(tpt), name), args) => // add outer arg to ctor call
        treeCopy.Apply(tree, super.transform(s), transformTrees(args :+ Ident("this")))

      case Apply(fun, args) => // add Function to by name params
        val params = tree.symbol.paramss.flatten
        treeCopy.Apply(tree, transform(fun), (args zip params) map { i =>
          if(definitions.isByNameParamType(i._2.tpe))
            treeCopy.Function(tree, Nil, transform(returnWrap(tree, i._1)))
          else
            transform(i._1)
        })

      case Select(Super(qual, mix), name) => // add call to super
        treeCopy.Apply(tree, treeCopy.Select(tree, super.transform(tree), "call"), List(Ident("this")))

      case s @ Select(qual, name) if(s.symbol.isMethod && s.symbol.paramss.isEmpty
          && !s.symbol.name.isOperatorName && !s.symbol.hasAccessorFlag) =>
            // prepend apply to Select method
        treeCopy.Apply(tree, super.transform(tree), Nil)

      case i @ Ident(name) if((i.symbol.isMethod && i.symbol.paramss.isEmpty)
          || i.symbol.isByNameParam) => // prepend apply to Ident method
        treeCopy.Apply(tree, super.transform(tree), Nil)

      case _ =>
        super.transform(tree)
    }
    }

    // prepend return to method body
    def returnWrap(owner: Tree, body: Tree) = {
      if(isUnitish(body))
        body
      else
        treeCopy.Return(owner, body)
    }

  }
}
