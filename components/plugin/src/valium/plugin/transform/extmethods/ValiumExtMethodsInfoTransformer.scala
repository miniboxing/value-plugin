package valium.plugin
package transform
package extmethods

import scala.tools.nsc.transform.InfoTransform

trait ValiumExtMethodsInfoTransformer extends InfoTransform {
  this: ValiumExtMethodsPhase =>

  import global._

  override def transformInfo(sym: Symbol, tpe: Type): Type = tpe
}