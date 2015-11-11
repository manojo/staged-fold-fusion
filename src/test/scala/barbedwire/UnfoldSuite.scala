package barbedwire

import scala.lms.common._
import scala.lms.internal.Effects
import lms._
import lms.util._
import lms.testutil.FileDiffSpec

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/**
 * Basic test suite for foldleft
 */

trait UnfoldProg
    extends Unfolds
    with FoldLefts
    with Equal
    with StringOps {

  /**
   * plugging a list iterator into a fold
   * gives us a loop that iterates over the list.
   */
  def unfoldId(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val fld = listIterator(xs).toFold
    fld.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map, test if it inlines
   */
  def map(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val mapped = listIterator(xs) map (_ * unit(2))

    mapped.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map map, test if it inlines
   */
  def mapmap(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))

    val mapped = listIterator(xs) map (_ * unit(2)) map (_ + unit(1))
    mapped.toFold.apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * map map over a range
   */
  def mapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeIterator(a, b)
    val mapped = xs map (_ * unit(2))
    (mapped map (_ + unit(1))).toFold.apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * filter over a range
   */
  def filterRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeIterator(a, b)
    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.toFold.apply[List[Int]](List[Int](), (ls, x) =>
      if (x.isDefined) ls ++ List(x.get)
      else ls
    )
  }

  /**
   * filter map over a range
   */
  def filtermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeIterator(a, b)
    val filterMapped = xs.filter(_ % unit(2) == unit(1)).map(x =>
      if (x.isDefined) Some(x.get * unit(3)) else none[Int]()
    )
    filterMapped.toFold.apply[List[Int]](List[Int](), (ls, x) =>
      if (x.isDefined) ls ++ List(x.get)
      else ls
    )
  }
}

/**
 * A trait that mixes all the relevant Exp traits that are required for this example
 * The corresponding codegen trait as well
 */
trait UnfoldExp
  extends FoldLeftExp
  with EqualExpOpt
  with StringOpsExp
  with OptionOpsExp

trait UnfoldGen
  extends FoldLeftGen
  with ScalaGenEqual
  with ScalaGenOptionOps {
  val IR: UnfoldExp
}

class UnfoldSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `main API generate code with no diff` = {
    withOutFile(prefix + "unfold") {
      new UnfoldProg
          with UnfoldExp
          with TupleOpsExp
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new UnfoldGen with ScalaGenTupleOps { val IR: self.type = self }

        codegen.emitSource(unfoldId _, "unfoldId", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcUnfoldId = compile(unfoldId)
        scala.Console.println(testcUnfoldId(1))
        codegen.reset

        codegen.emitSource(map _, "map", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMap = compile(map)
        scala.Console.println(testcMap(1))
        codegen.reset

        codegen.emitSource(mapmap _, "mapmap", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapmap = compile(mapmap)
        scala.Console.println(testcMapmap(1))
        codegen.reset

        codegen.emitSource2(mapmapRange _, "mapmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapmapRange = compile2(mapmapRange)
        scala.Console.println(testcMapmapRange(1, 5))
        codegen.reset

        codegen.emitSource2(filterRange _, "filterRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFilterRange = compile2(filterRange)
        scala.Console.println(testcFilterRange(1, 5))
        codegen.reset

        codegen.emitSource2(filtermapRange _, "filtermapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFiltermapRange = compile2(filtermapRange)
        scala.Console.println(testcFiltermapRange(1, 5))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "unfold")
  }
}
