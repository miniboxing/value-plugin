package valium.plugin
package transform
package addext

import scala.tools.nsc.transform.TypingTransformers

trait ValiumConvertTreeTransformer extends TypingTransformers {
  this: ValiumConvertPhase =>

  import global._
  import valium._

  class TreeConvertor(unit: CompilationUnit) extends TypingTransformer(unit) {
    // TODO: Convert to fields
    override def transform(tree: Tree): Tree = tree
  }
}