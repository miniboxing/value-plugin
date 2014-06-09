package valium.plugin
package transform
package coerce

import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.Phase
import scala.reflect.internal.Mode
import scala.reflect.internal.Mode._
import scala.util.DynamicVariable
import scala.reflect.internal.Flags._

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
      tree.foreach(node => if (node.tpe == null) unit.error(node.pos, s"[valium-coerce] tree not typed: $tree"))
      def isFlapping(tree: Tree) = tree match {
        case Unbox2box(Box2unbox(_)) => true
        case Box2unbox(Unbox2box(_)) => true
        case _ => false
      }
      tree.collect{ case sub if isFlapping(sub) => unit.error(sub.pos, s"unexpected leftovers after coerce: $sub") }
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
          val convertee = if (oldTpe.typeSymbol.isBottomClass) gen.mkAttributedCast(tree, newTpe.toBoxedValiumRef) else tree
          val tree1 = atPos(tree.pos)(Apply(gen.mkAttributedRef(conversion), List(convertee)))
          val tree2 = super.typed(tree1, mode, pt)
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

        def fallback() = super.typed(tree, mode, pt)
        def retypecheck() = super.typed(tree.clearType(), mode, pt)

        val res = tree match {
          case EmptyTree | TypeTree() =>
            fallback()

          case _ if tree.tpe == null =>
            fallback()

          // Intercept bm-s (unstable expressions of type unboxed multi-param value class) -- which are hard to translate
          // note that intercepting and boxing them simplifies the commit phase
          // Criteria:
          //  - it's a multi-param value class
          //  - pt is marked with @unboxed
          //  - is a b (=!isA(_))
          case _ if pt.valiumFields.length > 1 && pt.isUnboxedValiumRef && !looksLikeA(tree) && tree.symbol != box2unbox =>
            val tree2 = super.typed(tree.clearType(), mode, WildcardType)
            super.typed(Apply(gen.mkAttributedRef(box2unbox), List(tree)), mode, pt)


          // Intercept calls with the receiver being an unboxed value classe
          //  - if we can redirect to a
          case MaybeApply(MaybeTypeApply(sel @ Select(qual, meth), targs), args) if qual.isTerm && sel.symbol.isMethod =>
            val qual2 = super.typed(qual.clearType(), mode | QUALmode, WildcardType)
            if (qual2.isUnboxedValiumRef) {
              // for methods such as hashCode and ==, we need to redirect to the most specific symbol, since the
              // symbol is created later in the pipeline, therefore the typer marks the symbol as Object's hashCode.
              val objectMethod = sel.symbol
              val vclassMethod = objectMethod.matchingSymbol(qual2.tpe.typeSymbol, qual2.tpe)

              if (vclassMethod.isValiumMethodWithExtension) {
                // This is a value class method with an extension => redirect
                val extMethod = valiumAddExtPhase.extensionMethod(vclassMethod).asInstanceOf[Symbol]
                val allArgs = qual :: args
                val origThis = extMethod.owner.companionClass
                val baseType = qual.tpe.baseType(origThis)
                val allTargs = targs ::: baseType.typeArgs
                val fun = MaybeTypeApply(gen.mkAttributedSelect(gen.mkAttributedThis(extMethod.owner), extMethod), allTargs)
                val app = MaybeApply(fun, allArgs)
                val res = super.typed(app, mode, pt)
                res
              } else {
                // There is no corresponding extension method => box
                val tpe2 = if (qual2.tpe.hasAnnotation(UnboxedClass)) qual2.tpe else qual2.tpe.widen
                val tpe3 = tpe2.toBoxedValiumRef
                val qual3 = super.typed(qual.clearType(), mode, tpe3)
                super.typed(MaybeApply(MaybeTypeApply(Select(qual3, meth) setSymbol tree.symbol, targs), args), mode, pt)
              }
            } else {
              retypecheck()
            }

          case _ =>
            retypecheck()
        }

        adaptdbg(ind, " ==> " + res + ": " + res.tpe)
        if (res.tpe == ErrorType) adaptdbg(ind, "ERRORS: " + context.errors)
        indent -= 1
        res
      }
    }
  }
}