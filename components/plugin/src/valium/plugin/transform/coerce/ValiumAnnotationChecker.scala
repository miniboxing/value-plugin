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
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean = {
      assert(tpe1.withoutAnnotations <:< tpe2.withoutAnnotations)
      // we run localTyper during convert during code synthesis, and there we need `T @unboxed` and `Any` to be compatible again
      if (valiumCoercePhase != null && global.phase.id == valiumCoercePhase.id) {
        tpe1.isUnboxedValiumRef == tpe2.isUnboxedValiumRef || tpe1.isWildcard || tpe2.isWildcard
      } else {
        true
      }
    }
  }
}
