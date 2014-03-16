package valium.plugin
package transform

import metadata._
import verify._
import inject._
import coerce._
import convert._
import addext._

/** Removes the known problems in the Scala ASTs that cause the plugin
 *  to malfunction. For example: tailcall introduces .asInstancOf-s that
 *  prevent proper transformation and thus crash in the backend. */
trait ValiumPreparePhase extends
    ValiumPluginComponent
    with scala.tools.nsc.transform.Transform
    with ValiumPrepareTreeTransformer { self =>
  import global._
  def valiumPreparePhase: StdPhase
  def afterPrepare[T](op: => T): T = global.exitingPhase(valiumPreparePhase)(op)
  def beforePrepare[T](op: => T): T = global.enteringPhase(valiumPreparePhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // [error] /Users/xeno_by/Projects/valium/tests/correctness/test/valium/partest/CompileTest.scala:30: [valium-verify] tree not typed: $anonfun.this.apply$mcV$sp()
      // [error]       Console.withErr(pa) {
      // [error]                           ^
      // [error] one error found
      // TODO: I've no idea why this happens - looks like an invalid tree produced by scalac
      tree.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[valium-verify] tree not typed: $tree"))
      new TreePreparer(unit).transform(tree)
    }
  }
}

/** Makes sure that valium class definitions satisfy certain preconditions. */
trait ValiumVerifyPhase extends
    ValiumPluginComponent
    with scala.tools.nsc.transform.Transform
    with ValiumVerifyTreeTransformer { self =>
  import global._
  def valiumVerifyPhase: StdPhase
  def afterVerify[T](op: => T): T = global.exitingPhase(valiumVerifyPhase)(op)
  def beforeVerify[T](op: => T): T = global.enteringPhase(valiumVerifyPhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // [error] /Users/xeno_by/Projects/valium/tests/correctness/test/valium/partest/CompileTest.scala:30: [valium-verify] tree not typed: $anonfun.this.apply$mcV$sp()
      // [error]       Console.withErr(pa) {
      // [error]                           ^
      // [error] one error found
      // TODO: I've no idea why this happens - looks like an invalid tree produced by scalac
      tree.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[valium-verify] tree not typed: $tree"))
      new TreeVerifier(unit).traverse(tree)
      tree
    }
  }
}

/** Transforms `C` to `C @value` where appropriate (arguments of methods, local and field values, returns types of 1-param valium classes) */
trait ValiumInjectPhase extends
    ValiumPluginComponent
    with ValiumInjectInfoTransformer
    with ValiumInjectTreeTransformer { self =>
  import global._
  def valiumInjectPhase: StdPhase
  def afterInject[T](op: => T): T = global.exitingPhase(valiumInjectPhase)(op)
  def beforeInject[T](op: => T): T = global.enteringPhase(valiumInjectPhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterInject(new TreeInjector(unit).transform(tree))
      tree1.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[valium-inject] tree not typed: $tree"))
      tree1
    }
  }
}

/** Adds box2unbox and unbox2box coercions based on annotations injected during the previous phase */
trait ValiumCoercePhase extends
    ValiumPluginComponent
    with ValiumCoerceTreeTransformer
    with ValiumAnnotationCheckers { self =>
  import global._
  def valiumCoercePhase: StdPhase
  def afterCoerce[T](op: => T): T = global.exitingPhase(valiumCoercePhase)(op)
  def beforeCoerce[T](op: => T): T = global.enteringPhase(valiumCoercePhase)(op)
}

/** Representation conversion phase `C @value -> fields` */
trait ValiumConvertPhase extends
    ValiumPluginComponent
    with ValiumConvertInfoTransformer
    with ValiumConvertTreeTransformer { self =>
  import global._
  import helper._
  def valiumConvertPhase: StdPhase
  def afterConvert[T](op: => T): T = global.exitingPhase(valiumConvertPhase)(op)
  def beforeConvert[T](op: => T): T = global.enteringPhase(valiumConvertPhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterConvert(new TreeConverter(unit).transform(tree))
      tree1.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[valium-convert] tree not typed: $tree"))
      def isDisallowed(tree: Tree) = afterConvert(tree.symbol == box2unbox || tree.symbol == unbox2box || tree.symbol.isUnboxedValiumRef || tree.isUnboxedValiumRef)
      tree1.collect{ case sub if isDisallowed(sub) => unit.error(sub.pos, s"unexpected leftovers after convert: $sub") }
      tree1
    }
  }
}

/** Extension methods extractor */
trait ValiumAddExtensionMethodsPhase extends
    ValiumPluginComponent
    with ValiumAddExtInfoTransformer
    with ValiumAddExtTreeTransformer { self =>
  import global._
  def valiumAddExtPhase: StdPhase
  def afterAddExt[T](op: => T): T = global.exitingPhase(valiumAddExtPhase)(op)
  def beforeAddExt[T](op: => T): T = global.enteringPhase(valiumAddExtPhase)(op)

  override def newTransformer(unit: CompilationUnit): Transformer = new Transformer {
    override def transform(tree: Tree) = {
      // execute the tree transformer after all symbols have been processed
      val tree1 = afterAddExt(new TreeTransformer(unit).transform(tree))
      tree1.foreach(tree => if (tree.tpe == null && !tree.toString.contains("apply$mcV$sp")) unit.error(tree.pos, s"[valium-addext] tree not typed: $tree"))
      tree1
    }
  }
}
