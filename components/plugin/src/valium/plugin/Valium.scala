package valium.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent

import transform._
import metadata._

/** Main miniboxing class */
class Valium(val global: Global) extends Plugin { plugin =>
  // import global._

  val name = "valium"
  val description = "provides value class functionality"

  val components = List[PluginComponent](
    // TODO: another phase that does checks of @valium-annotated classes
    ValiumInjectPhaseObj,
    ValiumCoercePhaseObj,
    ValiumConvertPhaseObj,
    ValiumAddExtensionMethodsPhaseObj
  )

  // LDL adaptation
  global.addAnnotationChecker(ValiumCoercePhaseObj.ValueAnnotationChecker)

  lazy val helper = new { val global: plugin.global.type = plugin.global } with ValiumHelper
  var logValium = sys.props.get("valium.log").isDefined

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.toLowerCase() == "log")
        logValium = true
      else
        error("Valium: Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some(
    s"  -P:${name}:log               log valium transformations\n")

  private object ValiumInjectPhaseObj extends ValiumInjectPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("uncurry")
    val phaseName = Valium.this.name + "-inject"

    import global._
    val helper: plugin.helper.type = plugin.helper
    def logValium = Valium.this.logValium

    var valiumInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumInjectPhase = new Phase(prev)
      valiumInjectPhase
    }
  }

  private object ValiumCoercePhaseObj extends ValiumCoercePhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumInjectPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-coerce"

    import global._
    val helper: plugin.helper.type = plugin.helper
    def logValium = Valium.this.logValium

    var valiumCoercePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumCoercePhase = new CoercePhase(prev)
      valiumCoercePhase
    }
  }

  private object ValiumConvertPhaseObj extends ValiumConvertPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumCoercePhaseObj.phaseName)
    val phaseName = Valium.this.name + "-convert"

    import global._
    val helper: plugin.helper.type = plugin.helper
    def logValium = Valium.this.logValium

    var valiumConvertPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumConvertPhase = new Phase(prev)
      valiumConvertPhase
    }
  }

  private object ValiumAddExtensionMethodsPhaseObj extends ValiumAddExtensionMethodsPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumConvertPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-addext"

    import global._
    val helper: plugin.helper.type = plugin.helper
    def logValium = Valium.this.logValium

    var valiumAddExtPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumAddExtPhase = new Phase(prev)
      valiumAddExtPhase
    }
  }
}
