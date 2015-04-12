package lms.util

import lms._

import scala.virtualization.lms.common._
import scala.virtualization.lms.internal.GenericCodegen
import scala.reflect.SourceContext

import java.io.PrintWriter

/**
 * Inspired from TupleOps in the delite-develop branch of LMS
 */

trait EitherOps extends Base with IfThenElse with BooleanOps with Equal {
  import scala.language.implicitConversions

  implicit def make_either[A: Manifest, B: Manifest](o: Either[Rep[A], Rep[B]])(implicit pos: SourceContext): Rep[Either[A, B]]

  implicit class EitherOpsCls[A: Manifest, B: Manifest](o: Rep[Either[A, B]]) {

    /**
     * the "pattern" match
     */
    def isLeft: Rep[Boolean] = struct_isLeft(o)

    /**
     * map on Either
     */
    def map[C: Manifest, D: Manifest](l: Rep[A] => Rep[C], r: Rep[B] => Rep[D]): Rep[Either[C, D]] =
      if (o.isLeft) left[C, D](l(o.getLeft)) else right[C, D](r(o.getRight))

    def getLeft: Rep[A] = struct_getLeft(o)
    def getRight: Rep[B] = struct_getRight(o)
  }

  /**
   * operations handled by the Exp world
   */
  def left[A: Manifest, B: Manifest](a: Rep[A]): Rep[Either[A, B]]
  def right[A: Manifest, B: Manifest](b: Rep[B]): Rep[Either[A, B]]

  def struct_isLeft[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[Boolean]
  def struct_getLeft[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[A]
  def struct_getRight[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[B]

}

trait EitherOpsExp extends EitherOps with IfThenElseExpOpt with BooleanOpsExpOpt with StructOpsExpOpt
    with CastingOpsExp with EqualExpOpt {

  import scala.language.implicitConversions

  implicit def make_either[A: Manifest, B: Manifest](o: Either[Rep[A], Rep[B]])(implicit pos: SourceContext): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> o.left.getOrElse(rep_asinstanceof(unit(null), manifest[Null], manifest[A])),
      "right" -> o.right.getOrElse(rep_asinstanceof(unit(null), manifest[Null], manifest[B])),
      "isLeft" -> unit(o match { case Left(_) => true; case _ => false })
    )

  def none[T: Manifest](): Rep[Option[T]] =
    struct(classTag[Option[T]], "value" -> rep_asinstanceof(unit(null), manifest[Null], manifest[T]), "defined" -> unit(false))

  def left[A: Manifest, B: Manifest](a: Rep[A]): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> a,
      "right" -> rep_asinstanceof(unit(null), manifest[Null], manifest[B]),
      "isLeft" -> unit(true)
    )

  def right[A: Manifest, B: Manifest](b: Rep[B]): Rep[Either[A, B]] =
    struct(classTag[Either[A, B]],
      "left" -> rep_asinstanceof(unit(null), manifest[Null], manifest[A]),
      "right" -> b,
      "isLeft" -> unit(false)
    )

  def struct_isLeft[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[Boolean] = field[Boolean](e, "isLeft")
  def struct_getLeft[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[A] = field[A](e, "left")
  def struct_getRight[A: Manifest, B: Manifest](e: Rep[Either[A, B]]): Rep[B] = field[B](e, "right")
}

trait EitherGenBase extends GenericCodegen with BaseGenStructOps {
  val IR: EitherOpsExp

  override def remap[A](m: Manifest[A]) = m.erasure.getSimpleName match {
    case "Either" => IR.structName(m)
    case _ => super.remap(m)
  }
}

trait ScalaGenEitherOps extends ScalaGenBase with EitherGenBase with ScalaGenStructOps
  with ScalaGenCastingOps with ScalaGenEqual with ScalaGenIfThenElse { val IR: EitherOpsExp }

trait CGenEitherOps extends CGenBase with EitherGenBase with CGenStructOps with CGenCastingOps
  with CGenEqual with CGenIfThenElse { val IR: EitherOpsExp }

