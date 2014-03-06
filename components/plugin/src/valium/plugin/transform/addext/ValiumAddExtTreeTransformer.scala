package valium.plugin
package transform
package addext

import scala.tools.nsc.transform.TypingTransformers

trait ValiumAddExtTreeTransformer extends TypingTransformers {
  this: ValiumAddExtensionMethodsPhase =>

  import global._

  class TreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    // TODO: Extract extension methods
    override def transform(tree: Tree): Tree = tree
  }
}