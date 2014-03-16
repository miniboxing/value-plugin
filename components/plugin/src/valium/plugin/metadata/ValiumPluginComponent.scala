package valium.plugin.metadata

import scala.language.implicitConversions
import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.TypingTransformers

trait ValiumPluginComponent extends PluginComponent with TypingTransformers { self =>
  import global._
  import Flag._

  val helper: ValiumHelper { val global: self.global.type }
  import helper._
  def valiumlog(msg: => String) = if (settings.log.value.contains(phaseName)) log(msg)

  abstract class TreeRewriter(unit: CompilationUnit) extends TypingTransformer(unit) {
    sealed trait Result
    case class Single(tree: Tree) extends Result { override def toString = tree.toString }
    case class Multi(trees: List[Tree]) extends Result { override def toString = trees.toString }
    implicit def treeToResult(tree: Tree): Result = Single(tree)
    implicit def treesToResult(trees: List[Tree]): Result = Multi(trees)

    def typed(tree: Tree) = {
      try localTyper.typed(tree)
      catch { case err: TypeError => println(tree); println(showRaw(tree, printIds = true, printTypes = true)); throw err }
    }

    override def transform(tree: Tree): Tree = {
      val treeString = if (settings.log.value.contains(phaseName)) enteringPhase(globalPhase)(tree.toString) else ""
      val treeTpeString = if (settings.log.value.contains(phaseName)) s"${tree.tpe}" else ""
      def commit(rule: String, tree1: Result): Result = {
        def logRewrite() = valiumlog(s"$rule) $treeString -> $tree1")
        def logRetype() = valiumlog(s"$rule) $treeString: $treeTpeString -> ${tree1.asInstanceOf[Single].tree.tpe}")
        val tree2 = tree1 match {
          case Single(tree1) => if (tree ne tree1) logRewrite(); if ((tree eq tree1) && (tree.tpe ne tree1.tpe)) logRetype(); typed(tree1)
          case Multi(trees1) => logRewrite(); typed(Block(trees1: _*))
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
      catch { case err: TypeError => println(stats); println(showRaw(stats, printIds = true, printTypes = true)); throw err }
    }

    override def transformStats(stats: List[Tree], exprOwner: Symbol): List[Tree] = {
      stats flatMap {
        case stat =>
          val statString = if (settings.log.value.contains(phaseName)) enteringPhase(globalPhase)(stat.toString) else ""
          val statTpeString = if (settings.log.value.contains(phaseName)) s"${stat.tpe}" else ""
          def commit(rule: String, stats1: Result): Result = {
            def logRewrite() = valiumlog(s"$rule) $statString -> $stats1")
            def logRetype() = valiumlog(s"$rule) $statString: $statTpeString -> ${stats1.asInstanceOf[Single].tree.tpe}")
            val stats2 = stats1 match {
              case Single(stat1) => if (stat ne stat1) logRewrite(); if ((stat eq stat1) && (stat.tpe ne stat1.tpe)) logRetype(); List(typed(stat1))
              case Multi(stats1) => logRewrite(); typedStats(stats1, exprOwner)
            }
            transformStats(stats2, exprOwner)
          }
          def fallback(): Result = {
            if (exprOwner != currentOwner && stat.isTerm) atOwner(exprOwner)(super.transform(stat))
            else super.transform(stat)
          }
          rewrite(stat)(State(stat, if (stat.isTerm) exprOwner else currentOwner, commit _, fallback _)).lift(stat).getOrElse(fallback()) match {
            case Single(stat1) => List(stat1)
            case Multi(stats1) => stats1
          }
      }
    }

    case class State(tree: Tree, owner: Symbol, commit: (String, Result) => Result, fallback: () => Result) {
      def temp(name: TermName, rhs: Tree): ValDef = temp(name, rhs.tpe.widen, rhs)
      def temp(name: TermName, tpt: Tree, rhs: Tree): ValDef = temp(name, tpt.tpe, rhs)
      def temp(name: TermName, tpe: Type, rhs: Tree): ValDef = {
        val pos = tree.pos
        val flags = (if (tree.isDef) tree.symbol.flags else NoFlags) | SYNTHETIC
        val newsym = owner.newTermSymbol(name, pos, flags)
        newsym setInfo tpe
        if (newsym.owner.info.decls != EmptyScope) newsym.owner.info.decls.enter(newsym)
        atPos(pos)(newValDef(newsym, rhs)())
      }
      def explode(v: Symbol, x: Symbol, rhs: Tree): ValDef = explode(v, x, rhs.tpe.widen, rhs)
      def explode(v: Symbol, x: Symbol, tpt: Tree, rhs: Tree): ValDef = explode(v, x, tpt.tpe, rhs)
      def explode(v: Symbol, x: Symbol, tpe: Type, rhs: Tree): ValDef = {
        val name = nme.valueExplode(v, x)
        val exploded = temp(name, tpe, rhs)
        valiumlog(s"EXPLODE: $v -> ${exploded.symbol}")
        v.registerExploded(exploded.symbol)
        exploded
      }
      def explode(p: Symbol, x: Symbol): Symbol = {
        val exploded = p.owner.newSyntheticValueParam(p.info.memberInfo(x).finalResultType, nme.paramExplode(p, x))
        p.registerExploded(exploded)
        valiumlog(s"EXPLODE: $p -> $exploded")
        exploded
      }
      // def recur(tree: Tree): Tree = if (tree.isTerm) atOwner(owner)(transform(tree)) else transform(tree)
      // def recur(trees: List[Tree]): List[Tree] = transformStats(trees, owner)
      def error(msg: String): Result = { unit.error(tree.pos, msg); fallback() }
    }

    def commit(rule: String, result: Result)(implicit state: State): Result = state.commit(rule, result)
    def fallback()(implicit state: State): Result = state.fallback()
    def error(msg: String)(implicit state: State): Result = state.error(msg)
    // def recur(tree: Tree)(implicit state: State): Tree = state.recur(tree)
    // def recur(trees: List[Tree])(implicit state: State): List[Tree] = state.recur(trees)
    def temp(name: TermName, rhs: Tree)(implicit state: State): ValDef = state.temp(name, rhs)
    def temp(name: TermName, tpe: Type, rhs: Tree)(implicit state: State): ValDef = state.temp(name, tpe, rhs)
    def temp(name: TermName, tpt: Tree, rhs: Tree)(implicit state: State): ValDef = state.temp(name, tpt, rhs)
    def explode(p: Symbol, x: Symbol)(implicit state: State): Symbol = state.explode(p, x)
    def explode(v: Symbol, x: Symbol, rhs: Tree)(implicit state: State): ValDef = state.explode(v, x, rhs)
    def explode(v: Symbol, x: Symbol, tpe: Type, rhs: Tree)(implicit state: State): ValDef = state.explode(v, x, tpe, rhs)
    def explode(v: Symbol, x: Symbol, tpt: Tree, rhs: Tree)(implicit state: State): ValDef = state.explode(v, x, tpt, rhs)

    def rewrite(tree: Tree)(implicit state: State): PartialFunction[Tree, Result]
  }
}
