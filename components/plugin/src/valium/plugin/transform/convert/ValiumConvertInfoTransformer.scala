package valium.plugin
package transform
package convert

import scala.tools.nsc.transform.InfoTransform

trait ValiumConvertInfoTransformer extends InfoTransform {
  self: ValiumConvertPhase =>

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
