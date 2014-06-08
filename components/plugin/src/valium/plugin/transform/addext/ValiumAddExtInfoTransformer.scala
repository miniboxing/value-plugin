package valium.plugin
package transform
package addext

import scala.tools.nsc.transform.InfoTransform

trait ValiumAddExtInfoTransformer extends InfoTransform {
  self: ValiumAddExtPhase =>

  import global._
  import definitions._

  override def transformInfo(sym: Symbol, tpe: Type): Type = tpe
}
