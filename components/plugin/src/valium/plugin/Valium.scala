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
    ValiumPreparePhaseObj,
    ValiumVerifyPhaseObj,
    ValiumAddExtPhaseObj,
    ValiumInjectPhaseObj,
    ValiumCoercePhaseObj,
    ValiumConvertPhaseObj
  )

  // LDL adaptation
  global.addAnnotationChecker(ValiumCoercePhaseObj.ValueAnnotationChecker)

  lazy val helper = new { val global: plugin.global.type = plugin.global } with ValiumHelper

  override def processOptions(options: List[String], error: String => Unit) {
    for (option <- options) {
      error("Valium: option not understood: " + option)
    }
  }

  private object ValiumPreparePhaseObj extends ValiumPreparePhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List("refchecks")
    override val runsRightAfter = Some("specialize")
    val phaseName = Valium.this.name + "-prepare"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var valiumPreparePhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumPreparePhase = new Phase(prev)
      valiumPreparePhase
    }
  }

  private object ValiumVerifyPhaseObj extends ValiumVerifyPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumPreparePhaseObj.phaseName)
    val phaseName = Valium.this.name + "-verify"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var valiumVerifyPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumVerifyPhase = new Phase(prev)
      valiumVerifyPhase
    }
  }

  private object ValiumAddExtPhaseObj extends ValiumAddExtPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumVerifyPhaseObj.phaseName)
    val phaseName = Valium.this.name + "-addext"

    import global._
    val helper: plugin.helper.type = plugin.helper

    var valiumExtMethodsPhase : StdPhase = _
    override def newPhase(prev: scala.tools.nsc.Phase): StdPhase = {
      valiumExtMethodsPhase = new Phase(prev)
      valiumExtMethodsPhase
    }
  }

  private object ValiumInjectPhaseObj extends ValiumInjectPhase { self =>
    val global: Valium.this.global.type = Valium.this.global
    val runsAfter = List()
    override val runsRightAfter = Some(ValiumAddExtPhaseObj.phaseName)
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

  object ValiumPlugin extends global.analyzer.AnalyzerPlugin {
    import global._
    import analyzer.Typer

    override def pluginsTypeSig(tpe: Type, typer: Typer, defTree: Tree, pt: Type): Type = {
      defTree match {
        case cdef @ ClassDef(Modifiers(_, _, List(Apply(Select(New(Ident(TypeName("value"))), _), _))), _, _, _) =>
          val clazz = defTree.symbol
          val annotations = defTree.symbol.annotations
          annotations.map(_.completeInfo)
          val containsValium = annotations.exists(_.tpe.typeSymbol == plugin.helper.ValiumClass)
          if (containsValium)
            typer.namer.enclosingNamerWithScope(clazz.owner.rawInfo.decls).ensureCompanionObject(cdef)
        case _ =>
      }
      tpe
    }
  }

  global.analyzer.addAnalyzerPlugin(ValiumPlugin)
}
