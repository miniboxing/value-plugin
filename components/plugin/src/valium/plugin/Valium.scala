package valium.plugin

import scala.tools.nsc.Global
import scala.tools.nsc.plugins.Plugin
import scala.tools.nsc.plugins.PluginComponent

import transform._

/** Main miniboxing class */
class Valium(val global: Global) extends Plugin {
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

  var flag_log = sys.props.get("valium.log").isDefined

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      if (option.toLowerCase() == "log")
        flag_log = true
      else
        error("Valium: Option not understood: " + option)
    }
  }

  override val optionsHelp: Option[String] = Some(
    s"  -P:${name}:log               log valium transformations\n")

  private object ValiumInjectPhaseObj extends ValiumInjectPhase {
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("uncurry")
    val phaseName = Valium.this.name + "-inject"

    var valiumInjectPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumInjectPhase = new Phase(prev)
      valiumInjectPhase
    }
  }

  private object ValiumCoercePhaseObj extends {
    val valium: ValiumInjectPhaseObj.type = ValiumInjectPhaseObj
  } with ValiumCoercePhase {
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumInjectPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-coerce"

    var valiumCoercePhase : StdPhase = _
    def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumCoercePhase = new CoercePhase(prev.asInstanceOf[valium.Phase])
      valiumCoercePhase
    }
  }

  private object ValiumConvertPhaseObj extends {
    val valium: ValiumInjectPhaseObj.type = ValiumInjectPhaseObj
  } with ValiumConvertPhase {
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumCoercePhaseObj.phaseName)
    val phaseName = Valium.this.name + "-convert"

    var valiumConvertPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumConvertPhase = new Phase(prev)
      valiumConvertPhase
    }
  }

  private object ValiumAddExtensionMethodsPhaseObj extends {
    val valium: ValiumInjectPhaseObj.type = ValiumInjectPhaseObj
  } with ValiumAddExtensionMethodsPhase {
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumConvertPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-addext"

    def flag_log = Valium.this.flag_log

    var valiumAddExtPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumAddExtPhase = new Phase(prev.asInstanceOf[valium.Phase])
      valiumAddExtPhase
    }
  }
}
