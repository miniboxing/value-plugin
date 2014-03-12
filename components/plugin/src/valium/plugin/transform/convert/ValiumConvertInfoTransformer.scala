package valium.plugin
package transform
package convert

import scala.tools.nsc.transform.InfoTransform

trait ValiumConvertInfoTransformer extends InfoTransform {
  self: ValiumConvertPhase =>

  import global._
  import definitions._
  import helper._

  override def transformInfo(sym: Symbol, tpe: Type): Type = {
    // need to transform:
    // 1) def foo(x: C @value) = ??? => def foo(x$x: Int, x$y: Int) = ???
    // 2) val x: C @value = ???      => val x$x: Int = ???; val x$y: Int = ???
    // 3) def foo: C @value = ???    => def foo: Int = ??? (only for 1-arg value classes)
    // 4) unbox2box(x).field         => x$field
    def logTransform(tpe1: Type): Type = {
      if (logValium && !(tpe =:= tpe1)) println(s"[valium-convert] $sym: $tpe -> $tpe1")
      tpe1
    }
    if (sym.isMethod) {
      // this handles case #1
      def explode(params: List[Symbol]): List[Symbol] = {
        // TODO: we don't need to worry about throwing away param symbols, because valium-based DMT's are prohibited in valium-verify
        // we need to ban p.type types though, but that should also be done in valium-verify
        def explode(p: Symbol): List[Symbol] = {
          p.info.valiumFields.map(f => sym.newSyntheticValueParam(p.info.memberInfo(f), TermName(p.name + "$" + f.name)))
        }
        params.flatMap(p => if (p.isValiumRef) explode(p) else List(p))
      }
      // this handles case #3
      def unboxret(tpe: Type): Type = {
        if (tpe.isValue && tpe.isSingleFieldValiumRef) tpe.memberInfo(tpe.valiumFields.head)
        else tpe
      }
      def loop(tpe: Type): Type = tpe match {
        case MethodType(params, restpe @ MethodType(_, _)) => MethodType(explode(params), loop(restpe))
        case MethodType(params, restpe)                    => MethodType(explode(params), unboxret(restpe))
        case NullaryMethodType(restpe)                     => NullaryMethodType(loop(restpe))
        case PolyType(tparams, restpe)                     => PolyType(tparams, loop(restpe))
      }
      logTransform(loop(tpe))
    } else {
      // case #2 doesn't need to be handled by an info transform
      // we just throw away that symbol completely, so we don't care
      // case #4 is about a tree transformation only
      tpe
    }
  }
}
