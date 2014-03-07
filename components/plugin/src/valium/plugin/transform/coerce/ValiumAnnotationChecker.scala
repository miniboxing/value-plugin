package valium.plugin
package transform
package coerce

trait ValiumAnnotationCheckers {
  this: ValiumCoercePhase =>

  import global._
  import valium._

  object ValueAnnotationChecker extends AnnotationChecker{

    /**
     *  LDL FTW -- Boil frog, boil!
     */
    override def annotationsConform(tpe1: Type, tpe2: Type): Boolean =
      if (global.phase.id > valiumCoercePhase.id) {
        val res = tpe1.isValue == tpe2.isValue
        // println("after: " + tpe1 + " <: " + tpe2 + " ==> " + res + " (phase = " + global.phase.name + " " + global.phase.id + "  " + mboxAdaptPhase.id + ")")
        res
      } else {
        // println("before: " + tpe1 + " <: " + tpe2 + " ==> true" + " (phase = " + global.phase.name + "  " + global.phase.id + "  " + mboxAdaptPhase.id +  ")")
        true
      }
  }
}
