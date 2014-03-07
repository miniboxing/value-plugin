package valium.plugin
package transform
package coerce

import scala.tools.nsc.transform.TypingTransformers
import scala.tools.nsc.typechecker.Analyzer
import scala.reflect.internal.Mode
import scala.util.DynamicVariable

trait ValiumCoerceTreeTransformer extends TypingTransformers {
  this: ValiumCoercePhase =>

  import global._
  import valium._

  class CoercePhase(prev: Phase) extends StdPhase(prev) {
    override def name = ValiumCoerceTreeTransformer.this.phaseName
    override def checkable = false
    def apply(unit: CompilationUnit): Unit = {

      object TreeAdapter extends {
        val global: ValiumCoerceTreeTransformer.this.global.type = ValiumCoerceTreeTransformer.this.global
      } with TreeAdapters

      val tree = afterCoerce(TreeAdapter.adapt(unit))
      tree.foreach(node => assert(node.tpe != null, node))
    }
  }

  abstract class TreeAdapters extends Analyzer {
    import global._

    var indent = 0

    def adapt(unit: CompilationUnit): Tree = {
      val context = rootContext(unit)
      val checker = new TreeAdapter(context)
      unit.body = checker.typed(unit.body)
      unit.body
    }

    var normalTyper = false

    def withNormalTyper[T](context: Context)(f: Typer => T): T = {
      val normalTyper0 = normalTyper
      normalTyper = true
      val res = f(newTyper(context))
      normalTyper = normalTyper0
      res
    }

    override def newTyper(context: Context): Typer =
      if (normalTyper) {
        super.newTyper(context)
      } else {
        new TreeAdapter(context)
      }

    def adaptdbg(ind: Int, msg: => String): Unit = {
     println("  " * ind + msg)
    }

    class TreeAdapter(context0: Context) extends Typer(context0) {
      override protected def finishMethodSynthesis(templ: Template, clazz: Symbol, context: Context): Template =
        templ

      def supertyped(tree: Tree, mode: Mode, pt: Type): Tree =
        super.typed(tree, mode, pt)

      override protected def adapt(tree: Tree, mode: Mode, pt: Type, original: Tree = EmptyTree): Tree = {
        val ind = indent
        val oldTpe = tree.tpe
        val newTpe = pt
        if (oldTpe.isValue ^ newTpe.isValue) {
          val hAnnot1 = oldTpe.isValue
          val hAnnot2 = newTpe.isValue
          adaptdbg(ind, "adapting:")
          adaptdbg(ind, s"$tree ::: ${oldTpe.dealiasWiden} vs ${pt.dealiasWiden} ($hAnnot1 vs $hAnnot2)")

          if (hAnnot1 && !hAnnot2) {
            val tree1 = gen.mkMethodCall(unbox2box.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree))
            adaptdbg(ind, "patching by inserting minibox2box: " + tree1)
            val tree2 = super.typed(tree1, mode, pt)
            assert(tree2.tpe != ErrorType, tree2)
            tree2

          } else if (!hAnnot1 && hAnnot2) {
            assert(pt.typeSymbol.tpeHK != null, pt)
            val tree1 = gen.mkMethodCall(box2unbox.asInstanceOf[Symbol], List(oldTree.tpe.typeSymbol.tpeHK), List(oldTree))
            adaptdbg(ind, "patching by inserting box2minibox: " + tree1)
            adaptdbg(ind, "old type: " + oldTpe)
            adaptdbg(ind, "old tree: " + oldTree)
            val tree2 = super.typed(tree1, mode, pt)
            assert(tree2.tpe != ErrorType, tree2)
            tree2

          } else if ((hAnnot1 == hAnnot2) && (hAnnot1 == true)) {
            // there's no need for adapting, but singleton types and annotations don't mix
            // (check out mb_nested_class_fifth.scala for an example that crashes without
            // this workaround)
            tree setType null
            val tree0 = withNormalTyper(context) { _.typed(tree, mode, WildcardType) }
            tree0 setType newTpe // just force the type and accept the -Ycheck complaints
            tree0
          } else {
            println()
            adaptdbg(ind, "Don't know how to adapt tree:")
            adaptdbg(ind, oldTree + " : " + oldTree.tpe)
            adaptdbg(ind, s"to $pt")
            adaptdbg(ind, "tree after typing: " + tree)
            println()
            adaptdbg(ind, "Error:")
            adaptdbg(ind, err.toString)
            adaptdbg(ind, "Types:")
            adaptdbg(ind, s"  found:    $oldTpe (with underlying type ${oldTpe.dealiasWiden})")
            adaptdbg(ind, s"  required: $newTpe (with underlying type ${newTpe.dealiasWiden})")
            println("Can't adapt tree. Bailing out.")
            sys.exit(1)
          }
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

          case Select(qual, mth) if qual.isValue =>
            val boxed = Apply(gen.mkAttributedRef(unbox2box), List(qual))
            super.typed(Select(boxed, mth), mode, pt)

          // IMPORTANT NOTE: This case follows from the problem of overloading methods
          // if method m(t: T) is not overloaded, the argument is type-checked with pt=T
          // if method m(t: T) is overloaded, the argument is type-checked with pt=WildcardType and then the correct
          //                                  method overload is chosen. But we don't want that -- the overload is okay,
          //                                  but we need to adapt method arguments correctly. So we override the Apply
          //                                  type-checking procedure to eliminate overload resolution
          case Apply(fun, args) if fun.symbol != unbox2box && tree.tpe != null && tree.tpe != ErrorType =>
            val nargs =
              for ((arg, desiredTpe) <- (args zip fun.tpe.paramTypes)) yield
                typed(arg, mode, desiredTpe)
            val nfun =
              super.typedOperator(fun)
            super.typed(Apply(nfun, nargs))

          // IMPORTANT NOTE: ErrorType should be allowed to bubble up, since there will certainly be
          // a silent_.typed ready to catch the error and perform the correct rewriting upstream.
          case _ if tree.tpe != null && tree.tpe != ErrorType =>
            val oldTree = tree.duplicate
            val oldTpe = tree.tpe
            tree.clearType()
            super.typed(tree, mode, pt)

          case _ =>
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