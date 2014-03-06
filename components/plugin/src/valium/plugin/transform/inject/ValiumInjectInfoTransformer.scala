package valium.plugin
package transform
package inject

import scala.tools.nsc.transform.InfoTransform

trait ValiumInjectInfoTransformer extends InfoTransform {
  self: ValiumInjectPhase =>

  import global._
  import definitions._
  import valium._

  override def transformInfo(sym: Symbol, tpe: Type): Type = tpe

  lazy val deepTransformation: TypeMap = new TypeMap {
    def apply(tpe: Type): Type = mapOver(tpe)
    override def mapOver(tpe: Type): Type = tpe match {
      case tpe if tpe.hasAnnotation(ValueClass) =>
        LongTpe
      case _ =>
        super.mapOver(tpe)
    }
  }
}
