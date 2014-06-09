/* NSC -- new Scala compiler
 * Copyright 2005-2013 LAMP/EPFL
 * @author Martin Odersky
 */

package valium.plugin
package transform
package addext

import scala.collection.{ mutable, immutable }
import scala.collection.mutable.ListBuffer
import scala.language.postfixOps
import scala.tools.nsc.typechecker.Analyzer
import scala.tools.nsc.ast
import scala.reflect.internal.Flags._

trait ValiumAddExtSynthetizeMethods {
  self: ValiumAddExtPhase =>

  import global._
  import analyzer._
  import Flag._
  import definitions._
  import CODE._
  import helper._

  private lazy val valueSymbols      = List(Any_hashCode, Any_equals)
  private lazy val caseValueSymbols  = Any_toString :: valueSymbols

  /** Add the synthetic methods to case classes.
   */
  class SyntheticMethods(clazz0: Symbol, context: Context) {

    private val synthesizer = new ClassMethodSynthesis(
      clazz0,
      newTyper( if (reporter.hasErrors) context makeSilent false else context )
    )
    import synthesizer._

    private var syntheticCanEqual = false

    /* that match { case _: this.C => true ; case _ => false }
     * where `that` is the given method's first parameter.
     *
     * An isInstanceOf test is insufficient because it has weaker
     * requirements than a pattern match. Given an inner class Foo and
     * two different instantiations of the container, an x.Foo and and a y.Foo
     * are both .isInstanceOf[Foo], but the one does not match as the other.
     */
    private def thatTest(eqmeth: Symbol): Tree = {
      Apply(TypeApply(Select(Ident(eqmeth.firstParam), nme.isInstanceOf_ ), List(TypeTree(clazz.tpe))), Nil)
    }

    /* (that.asInstanceOf[this.C])
     * where that is the given methods first parameter.
     */
    private def thatCast(eqmeth: Symbol): Tree =
      gen.mkCast(Ident(eqmeth.firstParam), clazz.tpe)

    /* The equality method core for case classes and inline clases.
     * 1+ args:
     *   (that.isInstanceOf[this.C]) && {
     *       val x$1 = that.asInstanceOf[this.C]
     *       (this.arg_1 == x$1.arg_1) && (this.arg_2 == x$1.arg_2) && ... && (x$1 canEqual this)
     *      }
     * Drop canBuildFrom part if class is final and canBuildFrom is synthesized
     */
    private def equalsCore(eqmeth: Symbol, accessors: List[Symbol]) = {
      val otherName = context.unit.freshTermName(clazz.name + "$")
      val otherSym  = eqmeth.newValue(otherName, eqmeth.pos, SYNTHETIC) setInfo clazz.tpe
      val pairwise  = accessors map (acc => fn(Apply(Select(mkThis, acc), Nil), nme.EQ, Apply(Select(Ident(otherSym), acc), Nil)))
      val tests     = if (clazz.isDerivedValueClass || clazz.isFinal && syntheticCanEqual) pairwise else pairwise

      thatTest(eqmeth) AND Block(
        ValDef(otherSym, thatCast(eqmeth)),
        AND(tests: _*)
      )
    }

    /* The equality method for value classes
     * def equals(that: Any) = (this.asInstanceOf[AnyRef]) eq that.asInstanceOf[AnyRef]) || {
     *   (that.isInstanceOf[this.C]) && {
     *    val x$1 = that.asInstanceOf[this.C]
     *    (this.underlying == that.underlying
     */
    private def equalsDerivedValueClassMethod(symbol: Symbol): Tree = {
      def equ(symbol: Symbol) = equalsCore(symbol, clazz.valiumFields.map(_.getter(clazz0)))
      if (symbol == NoSymbol)
        createMethod(nme.EQ, List(AnyTpe), BooleanTpe) { equ }
      else
        equ(symbol)
    }


    /* The hashcode method for value classes
     * def hashCode(): Int = this.underlying.hashCode
     */
    private def hashCodeDerivedValueClassMethod(symbol: Symbol): Tree = {
      def hash(symbol: Symbol) = clazz.valiumFields.map(field => Select(mkThisSelect(field.getter(clazz0)), nme.hashCode_))
              .foldLeft[Tree](Literal(Constant(clazz0.name.toString.hashCode()))){ case (pres, code) => Apply(Select(pres, nme.PLUS), List(code)) }
      if (symbol == NoSymbol)
        createMethod(nme.hashCode_, Nil, IntTpe) { m => hash(m) }
      else
        hash(symbol)
    }

    // Exposed methods
    def makeHashCode(symbol: Symbol = NoSymbol): Tree = hashCodeDerivedValueClassMethod(symbol)
    def makeEquals(symbol: Symbol = NoSymbol): Tree   = equalsDerivedValueClassMethod(symbol)
  }
}