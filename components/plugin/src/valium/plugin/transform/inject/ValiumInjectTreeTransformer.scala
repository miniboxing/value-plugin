package valium.plugin
package transform
package inject

import scala.tools.nsc.transform.TypingTransformers

trait ValiumInjectTreeTransformer extends TypingTransformers {
  this: ValiumInjectPhase =>

  import global._
  import valium._

  class TreeInjector(unit: CompilationUnit) extends TypingTransformer(unit) {
    // TODO: Convert to fields
    override def transform(tree: Tree): Tree = tree
  }
}