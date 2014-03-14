package valium.plugin.metadata

import scala.language.implicitConversions
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.TypingTransformers

trait ValiumPluginComponent extends PluginComponent with TypingTransformers { self =>
  import global._
  import Flag._

  val helper: ValiumHelper { val global: self.global.type }
  def valiumlog(msg: => String) = if (settings.log.value.contains(phaseName)) log(msg)

  abstract class TreeRewriter(unit: CompilationUnit) extends TypingTransformer(unit) {
    sealed trait Result
    case class Single(tree: Tree) extends Result { override def toString = tree.toString }
    case class Multi(trees: List[Tree]) extends Result { override def toString = trees.toString }
    implicit def treeToResult(tree: Tree): Result = Single(tree)
    implicit def treesToResult(trees: List[Tree]): Result = Multi(trees)

    def typed(tree: Tree) = {
      try localTyper.typed(tree)
      catch { case err: TypeError => println(tree); throw err }
    }

    override def transform(tree: Tree): Tree = {
      def commit(tree1: Result): Result = {
        valiumlog(s"$tree -> $tree1")
        val tree2 = tree1 match {
          case Single(tree1) => typed(tree1)
          case Multi(trees1) => typed(Block(trees1: _*))
        }
        transform(tree2)
      }
      def fallback(): Result = super.transform(tree)
      rewrite(tree)(State(tree, currentOwner, commit _, fallback _)).lift(tree).getOrElse(fallback()) match {
        case Single(tree1) => tree1
        case Multi(trees1) => Block(trees1: _*)
      }
    }

    def typedStats(stats: List[Tree], exprOwner: Symbol) = {
      try localTyper.typedStats(stats, exprOwner)
      catch { case err: TypeError => println(stats); throw err }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap {
        case stat =>
          def commit(stats1: Result): Result = {
            valiumlog(s"$stat -> $stats1")
            val stats2 = stats1 match {
              case Single(stat1) => List(typed(stat1))
              case Multi(stats1) => typedStats(stats1, exprOwner)
            }
            transformStats(stats2, exprOwner)
          }
          def fallback(): Result = {
            if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(super.transform(stat))
            else super.transform(stat)
          }
          rewrite(stat)(State(stat, exprOwner, commit _, fallback _)).lift(stat).getOrElse(fallback()) match {
            case Single(stat1) => List(stat1)
            case Multi(stats1) => stats1
          }
      }
    }

    case class State(tree: Tree, owner: Symbol, commit: Result => Result, fallback: () => Result) {
      def temp(name: TermName, rhs: Tree): ValDef = temp(name, rhs.tpe, rhs)
      def temp(name: TermName, tpt: Tree, rhs: Tree): ValDef = temp(name, tpt.tpe, rhs)
      def temp(name: TermName, tpe: Type, rhs: Tree): ValDef = {
        val pos = tree.pos
        val flags = (if (tree.isDef) tree.symbol.flags else NoFlags) | SYNTHETIC
        val newsym = owner.newTermSymbol(name, pos, flags)
        newsym setInfo tpe
        if (newsym.owner.info.decls != EmptyScope) newsym.owner.info.decls.enter(newsym)
        atPos(pos)(newValDef(newsym, rhs)())
      }
    }

    def commit(result: Result)(implicit state: State): Result = state.commit(result)
    def fallback()(implicit state: State): Result = state.fallback()
    def temp(name: TermName, rhs: Tree)(implicit state: State): ValDef = state.temp(name, rhs)
    def temp(name: TermName, tpe: Type, rhs: Tree)(implicit state: State): ValDef = state.temp(name, tpe, rhs)
    def temp(name: TermName, tpt: Tree, rhs: Tree)(implicit state: State): ValDef = state.temp(name, tpt, rhs)

    def rewrite(tree: Tree)(implicit state: State): PartialFunction[Tree, Result]
  }
}
