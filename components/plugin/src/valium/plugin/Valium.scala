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
    ValiumVerifyPhaseObj,
    ValiumInjectPhaseObj,
    ValiumCoercePhaseObj,
    ValiumConvertPhaseObj,
    ValiumAddExtensionMethodsPhaseObj
  )

  // LDL adaptation
  global.addAnnotationChecker(ValiumCoercePhaseObj.ValueAnnotationChecker)

  lazy val helper = new { val global: plugin.global.type = plugin.global } with ValiumHelper

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      error("Valium: option not understood: " + option)
    }
  }

  private object ValiumVerifyPhaseObj extends ValiumVerifyPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("specialize")
    val phaseName = Valium.this.name + "-verify"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var valiumVerifyPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumVerifyPhase = new VerifyPhase(prev)
      valiumVerifyPhase
    }
  }

  private object ValiumInjectPhaseObj extends ValiumInjectPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumVerifyPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-inject"

    import global._
    val helper: plugin.helper.type = plugin.helper

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

    var valiumAddExtPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumAddExtPhase = new Phase(prev)
      valiumAddExtPhase
    }
  }
}
