package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent
import scala.tools.nsc.transform.TypingTransformers

trait ValiumPluginComponent extends PluginComponent with TypingTransformers { self =>
  import global._

  val helper: ValiumHelper { val global: self.global.type }
  def valiumlog(msg: => String) = if (settings.log.value.contains(phaseName)) log(msg)

  abstract class ValuimTypingTransformer(unit: CompilationUnit) extends TypingTransformer(unit) {
    def localTyped(tree: Tree) = {
      try localTyper.typed(tree)
      catch {
        case err: TypeError =>
          println(tree)
          throw err
      }
    }

    def localTyped(stats: List[Tree], exprOwner: Symbol) = {
      try localTyper.typedStats(stats, exprOwner)
      catch {
        case err: TypeError =>
          println(stats)
          throw err
      }
    }
  }
}
