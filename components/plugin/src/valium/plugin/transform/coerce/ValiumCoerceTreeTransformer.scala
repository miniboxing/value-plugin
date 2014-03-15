package valium.plugin
package transform
package coerce

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Phase
import scala.reflect.internal.Mode
import scala.util.DynamicVariable

trait ValiumCoerceTreeTransformer {
  this: ValiumCoercePhase =>

  import global._
  import definitions._
  import helper._

  class CoercePhase(prev: Phase) extends StdPhase(prev) {
    override def name = ValiumCoerceTreeTransformer.this.phaseName
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {
      val tree = afterCoerce(new TreeAdapters().adapt(unit))
      tree.foreach(node => assert(node.tpe != null, node))
    }
  }

  class TreeAdapters extends Analyzer {
    var indent = 0
    def adaptdbg(ind: Int, msg: => String): Unit = valiumlog("  " * ind + msg)

    lazy val global: ValiumCoerceTreeTransformer.this.global.type = ValiumCoerceTreeTransformer.this.global
    override def newTyper(context: Context): Typer = new TreeAdapter(context)

    def adapt(unit: CompilationUnit): Tree = {
      val context = rootContext(unit)
      val checker = new TreeAdapter(context)
      unit.body = checker.typed(unit.body)
      unit.body
    }

    class TreeAdapter(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      def supertyped(tree: Tree, mode: Mode, pt: Type): Tree =
        super.typed(tree, mode, pt)

      override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
        val oldTpe = tree.tpe
        val newTpe = pt
        def typeMismatch = oldTpe.isUnboxedValiumRef ^ newTpe.isUnboxedValiumRef
        def dontAdapt = tree.isType || pt.isWildcard
        if (typeMismatch && !dontAdapt) {
          val conversion = if (oldTpe.isUnboxedValiumRef) unbox2box else box2unbox
          val tree1 = atPos(tree.pos)(Apply(gen.mkAttributedRef(conversion), List(tree)))
          val tree2 = super.typed(tree1, mode, pt)
//          println("adapted: " + tree + " to " + tree2 + "  tpe = " + oldTpe + " pt = " + pt)
          assert(tree2.tpe != ErrorType, tree2)
          tree2
        } else {
          super.adapt(tree, mode, pt, original)
        }
      }

      override def typed(tree: Tree, mode: Mode, pt: Type): Tree = {
        val ind = indent
        indent += 1
        adaptdbg(ind, " <== " + tree + ": " + showRaw(pt, true, true, false, false))


        val res = tree match {

          case EmptyTree | TypeTree() =>
            super.typed(tree, mode, pt)

          case _ if tree.tpe == null =>
            super.typed(tree, mode, pt)

          // intercept bm-s:
          //  - it's a multi-param value class
          //  - pt is marked with @unboxed
          //  - is a b (=!isA(_))
          case _ if (pt.valiumFields.length > 1) && pt.isUnboxedValiumRef && !looksLikeA(tree) && (tree.symbol != box2unbox) =>
//            println()
//            println("starting: " + tree)
            val tree2 = super.typed(tree.clearType(), mode, WildcardType)
            super.typed(Apply(gen.mkAttributedRef(box2unbox), List(tree)), mode, pt)

          case Select(qual, meth) if qual.isTerm && tree.symbol.isMethod =>
            val qual2 = super.typed(qual.setType(null), mode, WildcardType)
            if (qual2.isUnboxedValiumRef) {
              val tpe2 = if (qual2.tpe.hasAnnotation(UnboxedClass)) qual2.tpe else qual2.tpe.widen
              val tpe3 = tpe2.toBoxedValiumRef
              val qual3 = super.typed(qual.setType(null), mode, tpe3)
              super.typed(Select(qual3, meth) setSymbol tree.symbol, mode, pt)
            } else {
              tree.clearType()
              super.typed(tree, mode, pt)
            }

          case _ =>
            tree.clearType()
            super.typed(tree, mode, pt)
        }
        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
        if (res.tpe == ErrorType)
          adaptdbg(ind, "ERRORS: " + context.errors)
        indent -= 1
        res
      }
    }
  }
}