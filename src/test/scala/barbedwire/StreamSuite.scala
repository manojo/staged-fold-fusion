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

trait StreamProg
    extends Streams
    with Equal
    with StringOps {

  /**
   * plugging a list stream into a fold
   * gives us a loop that iterates over the list.
   */
  def streamId(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val fld = listStream(xs).toFold
    fld.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map, test if it inlines
   */
  def map(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val mapped = listStream(xs) map (_ * unit(2))

    mapped.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map map, test if it inlines
   */
  def mapmap(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))

    val mapped = listStream(xs) map (_ * unit(2)) map (_ + unit(1))
    mapped.toFold.apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * map map over a range
   */
  def mapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeStream(a, b)
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
    val xs = rangeStream(a, b)
    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * filter map over a range
   */
  def filtermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeStream(a, b)
    val filterMapped = xs.filter(_ % unit(2) == unit(1)).map(x => x * unit(3))
    filterMapped.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * filter followed by filter over a range
   * filter pumping out options
   */
  def filterfilterRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeStream(a, b)
    val odds = xs filter (_ % unit(2) == unit(1))
    val filtered = odds filter (_ > unit(3))

    filtered.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * the dot-product
   */
  def dotProductRange(a: Rep[Int], b: Rep[Int]): Rep[Int] = {
    val xs = rangeStream(a, unit(5))
    val ys = rangeStream(b, unit(5))

    val dotted = (xs zip ys).map(pair => pair.apply((x, y) => x * y))

    dotted.toFold.apply[Int](unit(0), (acc, x) => acc + x)
  }

  /**
   * A test to see what happens to `OptionCPS[Rep[Stream]]`
   */
//  def repStream(a: Rep[Int]): Rep[Int] = {
//    val bla: Rep[Stream[Int, Int]] =
//      mkStream(rangeStream(a, unit(10)))
//
//    bla.toFold.apply[Int](unit(0), (acc, x) => acc + x)
//  }

  /**
   * flatten over a range
   */
  def flattenRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeStream(a, b)
    val flattened = xs.flatten[Int, Int](
      a => a,
      s => s > unit(5),
      s => mkPair(mkSome(s), s + unit(1))
    )

    flattened.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * flatMap over a range
   */
  def flatMapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = rangeStream(a, b)
    val flatMapped = xs flatMap (i => rangeStream(unit(1), i))
    flatMapped.toFold.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }
}

trait StreamGen extends UnfoldGen with PairCPSGenBase {
  val IR: StreamExp
}

class StreamSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `main API generate code with no diff` = {
    withOutFile(prefix + "stream") {
      new StreamProg
          with StreamExp
          with TupleOpsExp
          ///** this trait should be mixed in higher up */ with ArrayOpsExp
          ///** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new StreamGen with ScalaGenTupleOps { val IR: self.type = self }

        codegen.emitSource(streamId _, "streamId", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcStreamId = compile(streamId)
        scala.Console.println(testcStreamId(1))
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

        codegen.emitSource2(filterfilterRange _, "filterfilterRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFilterfilterRange = compile2(filterfilterRange)
        scala.Console.println(testcFilterfilterRange(1, 5))
        codegen.reset

        codegen.emitSource2(dotProductRange _, "dotProductRange", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcdotProductRange = compile2(dotProductRange)
        scala.Console.println(testcdotProductRange(1, 1))
        scala.Console.println(testcdotProductRange(1, 4))
        scala.Console.println(testcdotProductRange(4, 1))
        codegen.reset

//        codegen.emitSource(repStream _, "repStream", new java.io.PrintWriter(System.out))
//        codegen.reset
//
//        val testcRepStream = compile(repStream)
//        scala.Console.println(testcRepStream(1))
//        scala.Console.println(testcRepStream(4))
//        codegen.reset

        codegen.emitSource2(flattenRange _, "flattenRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlattenRange = compile2(flattenRange)
        scala.Console.println(testcFlattenRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapRange _, "flatMapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapRange = compile2(flatMapRange)
        scala.Console.println(testcFlatMapRange(1, 5))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "stream")
  }
}
