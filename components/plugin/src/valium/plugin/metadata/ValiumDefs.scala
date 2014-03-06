package valium.plugin.metadata

import scala.tools.nsc.plugins.PluginComponent

trait ValiumDefs {
  this: PluginComponent =>

  import global._
  import definitions._

  lazy val ValiumClass = rootMirror.getRequiredClass("scala.valium")

  /**
   * This class should only appear in the tree during the `minibox` phase
   * and should be cleaned up afterwards, during the `minibox-cleanup` phase.
   */
  lazy val ValueClass = {
    // This is what is should look like:
    // ```
    //   package __root__.scala {
    //     class value extends Annotation with TypeConstraint
    //   }
    // ```
    val AnnotationName = "scala.annotation.Annotation"
    val TypeConstrName = "scala.annotation.TypeConstraint"
    val AnnotationTpe = rootMirror.getRequiredClass(AnnotationName).tpe
    val TypeConstrTpe = rootMirror.getRequiredClass(TypeConstrName).tpe

    val StorageName = newTypeName("value")
    val StorageSym = ScalaPackageClass.newClassSymbol(StorageName, NoPosition, 0L)
    StorageSym setInfoAndEnter ClassInfoType(List(AnnotationTpe, TypeConstrTpe), newScope, StorageSym)
    StorageSym
  }

  // artificially created marker methods
  lazy val marker_unbox2box =
    newPolyMethod(1, ScalaPackageClass, newTermName("marker_unbox2box"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK withAnnotation AnnotationInfo(ValueClass.tpe, Nil, Nil))), tpar.head.tpeHK))
  lazy val marker_box2unbox =
    newPolyMethod(1, ScalaPackageClass, newTermName("marker_box2unbox"), 0L)(
      tpar => (Some(List(tpar.head.tpeHK)), tpar.head.tpeHK withAnnotation AnnotationInfo(ValueClass.tpe, Nil, Nil)))
}