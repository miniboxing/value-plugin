package valium.plugin
package transform
package addext

trait ValiumAddExtTreeTransformer {
  this: ValiumAddExtensionMethodsPhase =>

  import global._
  import definitions._

  class TreeTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    override def transform(tree: Tree): Tree = tree
  }
}