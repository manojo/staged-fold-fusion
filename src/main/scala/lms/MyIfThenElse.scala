package lms

import scala.virtualization.lms.common._

import scala.virtualization.lms.util.OverloadHack
import scala.virtualization.lms.internal.{ GenericNestedCodegen, GenericFatCodegen }
import scala.reflect.{ SourceContext, RefinedManifest }

import java.io.PrintWriter

/**
 * Extra optimisations for IfThenElse. Based on code at https://github.com/manojo/experiments
 */
trait MyIfThenElseExpOpt
    extends IfThenElseExpOpt
    with BooleanOpsExp
    with EqualExpBridge {

  /**
   * A map that records conditions already seen. So that inner conditional expressions using
   * the same condition are eliminated.
   */
  val map = new scala.collection.mutable.HashMap[Rep[Boolean], Boolean]

  override def __ifThenElse[T: Manifest](cond: Rep[Boolean], thenp: => Rep[T], elsep: => Rep[T])
                                        (implicit pos: SourceContext) = cond match {
    case Const(true) => thenp
    case Const(false) => elsep
    case Def(BooleanNegate(a)) => __ifThenElse(a, elsep, thenp)
    case Def(NotEqual(a, b)) => __ifThenElse(equals(a, b), elsep, thenp)
    case _ =>
      if (map.contains(cond)) {
        if (map(cond)) thenp else elsep
      } else {
        val a = reifyEffectsHere {
          map += (cond) -> true
          //the by name parameter is now evaluated
          //thereby triggering possible nested ifThenElse-s
          val tmp = thenp
          map -= cond
          tmp
        }

        val b = reifyEffectsHere {
          map += (cond) -> false
          val tmp = elsep
          map -= cond
          tmp
        }

        ifThenElse(cond, a, b)
      }
  }
}


/**
 * An extension to ScalaGenIfThenElseFat that generates
 * multiple vars instead of tuples
 * @author @sstucki
 * source: https://github.com/sstucki/virtualization-lms-core/blob/develop/src/common/IfThenElse.scala
 */
trait MyScalaGenIfThenElseFat extends ScalaGenIfThenElseFat {

  import IR._

  override def emitFatNode(symList: List[Sym[Any]], rhs: FatDef) = rhs match {

    case SimpleFatIfThenElse(c, as, bs) =>
      def emitRetAssignments[T](vars: List[Sym[Any]], retVals: List[Exp[T]]) =
        (vars zip retVals) foreach { case (v, rv) => emitAssignment(v, quote(rv)) }

      if (symList.length > 1) {
        symList foreach emitForwardDef
        stream.println("if (" + quote(c) + ") {")
      } else stream.println("val " + symList.map(quote).mkString + " = if (" + quote(c) + ") {")

      emitFatBlock(as)

      if (symList.length > 1) emitRetAssignments(symList, as map getBlockResult)
      else stream.println((as map (a => quote(getBlockResult(a)))).mkString)

      stream.println("} else {")
      emitFatBlock(bs)

      if (symList.length > 1) emitRetAssignments(symList, bs map getBlockResult)
      else stream.println((bs map (b => quote(getBlockResult(b)))).mkString)

      stream.println("}")

    case _ => super.emitFatNode(symList, rhs)
  }

  /**
   * An extension to emitForwardDef to generate
   * relevant null values for primitive types
   *
   * sstucki: Ideally, `emitForwardDef` would be implemented as
   * follows:
   *
   * def emitForwardDef(sym: Sym[Any]) {
   * emitVarDef(sym.asInstanceOf[Sym[Variable[Any]]], "_")
   * }
   *
   * to generate code of the form
   *
   * var x$0: Int = _
   *
   * However, this isn't valid Scala (as of 2013-11-06) if `x$0` is a
   * local variable (scalac 2.10.3 emits "error: local variables must
   * be initialized"). As a workaround, we initialized the variables
   * to a suitable "zero" value. `ZeroVal[T]` handles value types
   * specially (rather than just casting `null` to `T`), which reduces
   * byte code overhead (initializing e.g. an `Int` to `null`
   * generates a hand-full of byte code ops to construct a boxed `Int`
   * and subsequently unbox the result to initialize the variable).
   */

  override def emitForwardDef(sym: Sym[Any]): Unit = {
    import lms.ZeroVal
    def quotedZero[A: Manifest]: String = quote(Const(ZeroVal[A]))
    stream.println("var " + quote(sym) + ": " + remap(sym.tp) + " = " + quotedZero(sym.tp))
  }
}
