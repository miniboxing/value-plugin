package valium.plugin
package transform
package coerce

trait ValiumAnnotationCheckers {
  this: ValiumCoercePhase =>

  import global._
  import definitions._
  import helper._

  object ValueAnnotationChecker extends AnnotationChecker{

    /**
     *  LDL FTW -- Boil frog, boil!
     */
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean =
      if (valiumCoercePhase != null && global.phase.id > valiumCoercePhase.id) {
        tpe1.isUnboxedValiumRef == tpe2.isUnboxedValiumRef || tpe1.isWildcard || tpe2.isWildcard
      } else {
        true
      }
  }
}
