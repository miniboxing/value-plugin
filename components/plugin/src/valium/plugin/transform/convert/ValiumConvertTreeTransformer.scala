package valium.plugin
package transform
package addext

import scala.tools.nsc.transform.TypingTransformers

trait ValiumConvertTreeTransformer extends TypingTransformers {
  this: ValiumConvertPhase =>

  import global._
  import definitions._

  class TreeConvertor(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree
  }
}