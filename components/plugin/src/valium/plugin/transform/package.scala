package valium.plugin
package transform

import scala.tools.nsc.plugins.PluginComponent
import metadata._
import inject._
import coerce._
import convert._
import addext._

/** Representation conversion phase `C @value -> fields` */
trait ValiumInjectPhase extends
    PluginComponent
    with ValiumInfo
    with ValiumDefs
    with ValiumInjectInfoTransformer
    with ValiumInjectTreeTransformer {

  def valiumInjectPhase: StdPhase

  def afterInject[T](op: => T): T = global.exitingPhase(valiumInjectPhase)(op)
  def beforeInject[T](op: => T): T = global.enteringPhase(valiumInjectPhase)(op)

  import global._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterInject(new TreeInjector(unit).transform(tree))
      tree1.foreach(tree => assert(tree.tpe != null, "tree not typed: " + tree))
      tree1
    }
  }
}

/** Coercion adder */
trait ValiumCoercePhase extends
    PluginComponent
    with ValiumCoerceTreeTransformer
    with ValiumAnnotationCheckers {

  val valium: ValiumInjectPhase { val global: ValiumCoercePhase.this.global.type }

  def valiumCoercePhase: StdPhase

  def afterCoerce[T](op: => T): T = global.exitingPhase(valiumCoercePhase)(op)
  def beforeCoerce[T](op: => T): T = global.enteringPhase(valiumCoercePhase)(op)
}

/** Representation conversion phase `C @value -> fields` */
trait ValiumConvertPhase extends
    PluginComponent
    with ValiumConvertInfoTransformer
    with ValiumConvertTreeTransformer {

  val valium: ValiumInjectPhase { val global: ValiumConvertPhase.this.global.type }

  def valiumConvertPhase: StdPhase

  def afterConvert[T](op: => T): T = global.exitingPhase(valiumConvertPhase)(op)
  def beforeConvert[T](op: => T): T = global.enteringPhase(valiumConvertPhase)(op)

  import global._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterConvert(new TreeConvertor(unit).transform(tree))
      tree1.foreach(tree => assert(tree.tpe != null, "tree not typed: " + tree))
      tree1
    }
  }
}

/** Extension methods extractor and @value annotation injector */
trait ValiumAddExtensionMethodsPhase extends
    PluginComponent
    with ValiumAddExtInfoTransformer
    with ValiumAddExtTreeTransformer {

  val valium: ValiumInjectPhase { val global: ValiumAddExtensionMethodsPhase.this.global.type }

  def valiumAddExtPhase: StdPhase

  def afterAddExt[T](op: => T): T = global.exitingPhase(valiumAddExtPhase)(op)
  def beforeAddExt[T](op: => T): T = global.enteringPhase(valiumAddExtPhase)(op)

  def flag_log: Boolean

  import global._

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterAddExt(new TreeTransformer(unit).transform(tree))
      tree1.foreach(tree => assert(tree.tpe != null, "tree not typed: " + tree))
      tree1
    }
  }
}
