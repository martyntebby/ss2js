package ss2js.plugin

import scala.tools.nsc.Global

/**
 * Flatten multiple method argument lists
 * TODO: and add empty one when none.
 * It is needed twice at present.
 */
class FlattenTrans[G <: Global](val global: G) extends TreeTrans {

  import global._

  val transformer = new Transformer {

    override def transform(tree: Tree): Tree =
    tree match {
      case Apply(Apply(fun, args1), args2) => // collapse multiple param lists
        transform(treeCopy.Apply(tree, fun, args1 ++ args2))

      case _ =>
        super.transform(tree)
    }
  }
}
