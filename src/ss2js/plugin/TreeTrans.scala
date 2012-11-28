package ss2js.plugin

import scala.tools.nsc.Global

abstract class TreeTrans extends Utility {
  val transformer: global.Transformer
  def transform(tree: global.Tree) = transformer.transform(tree)
}
