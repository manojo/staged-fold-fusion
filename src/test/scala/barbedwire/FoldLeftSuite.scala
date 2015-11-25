package barbedwire

import scala.lms.common._
import scala.lms.internal.Effects
import lms._
import lms.util._
import lms.testutil.FileDiffSpec

//only importing this to access the type
import scala.collection.immutable.HashMap

import java.io.PrintWriter
import java.io.StringWriter
import java.io.FileOutputStream

/**
 * Basic test suite for foldleft
 */

trait FoldLeftProg
    extends FoldLefts
    with Equal
    with MyHashMapOps
    with StringOps {

  /**
   * simple foldLeft back into a list
   */
  def foldLeftId(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val fld = FoldLeft.fromList[Int](xs)
    fld.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map, test if it inlines
   */
  def map(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))
    val mapped = FoldLeft.fromList[Int](xs) map (_ * unit(2))

    mapped.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * map map, test if it inlines
   */
  def mapmap(in: Rep[Int]): Rep[List[Int]] = {
    val xs = List(unit(1), unit(2), unit(3))

    val mapped = FoldLeft.fromList[Int](xs) map (_ * unit(2))
    (mapped map (_ + unit(1))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * map map over a range
   */
  def mapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val mapped = xs map (_ * unit(2))
    (mapped map (_ + unit(1))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * filter over a range
   */
  def filterRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * filter map over a range
   */
  def filtermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val filtered = xs filter (_ % unit(2) == unit(1))
    filtered.map(_ * unit(3)).apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * flatMap over a range
   */
  def flatMapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val flatMapped = xs flatMap (i => FoldLeft.fromRange(unit(1), i))
    flatMapped.apply[List[Int]](List[Int](), (ls, x) => ls ++ List(x))
  }

  /**
   * flatMap map over a range
   */
  def flatMapmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val flatMapped = xs flatMap (i => FoldLeft.fromRange(unit(1), i))
    flatMapped.map(_ * unit(2)).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * flatMap filter map over a range
   */
  def flatMapfiltermapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val fld = FoldLeft.fromRange(a, b)
    val flatMapped = fld flatMap (i => FoldLeft.fromRange(unit(1), i))
    val filtered = flatMapped filter (_ % unit(2) == unit(1))
    filtered.map(_ * unit(3)).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * flatMap filter map over a range, summing it up
   */
  def flatMapfiltermapRange2(a: Rep[Int], b: Rep[Int]): Rep[Int] = {
    val xs = FoldLeft.fromRange(a, b)
    val flatMapped = xs flatMap (i => FoldLeft.fromRange(unit(1), i))
    val filtered = flatMapped filter (_ % unit(2) == unit(1))
    filtered.map(_ * unit(3)).apply[Int](
      unit(0),
      (acc, x) => acc + x
    )
  }

  /**
   * map concat map over a range
   */
  def mapconcatmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val ys = FoldLeft.fromRange(a, b)
    val mapped = xs map (_ * unit(2))
    ((mapped ++ ys) map (_ * unit(3))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * map append map over a range
   */
  def mapappendmapRange(a: Rep[Int], b: Rep[Int]): Rep[List[Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val mapped = xs map (_ * unit(2))
    ((mapped :+ (b + unit(1))) map (_ * unit(3))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * partition map
   */
  def partitionmapRange(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange(a, b)
    val (evens, odds) = xs.partition(_ % unit(2) == unit(0))
    val (mappedEvens, mappedOdds) = (evens map (_ * unit(2)), odds map (_ * unit(3)))
    val evenList = (evens map (_ * unit(2))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )
    val oddList = (odds map (_ * unit(3))).apply[List[Int]](
      List[Int](),
      (ls, x) => ls ++ List(x)
    )

    make_tuple2(evenList, oddList)
  }

  /**
   * a "multifoldleft". fold over two sequences.
   * Just to see what happens
   */
  def multifoldleftRange(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange(a, b)
    xs.apply[(List[Int], List[Int])](
      make_tuple2((List[Int](), List[Int]())),
      (acc, elem) =>
        if (elem % unit(2) == unit(0)) make_tuple2((acc._1 ++ List(elem), acc._2))
        else make_tuple2((acc._1, acc._2 ++ List(elem)))
    )
  }

  /**
   *  partition bis. Just a really basic fold with nothing more
   */
  def partitionbis(a: Rep[Int], b: Rep[Int]): Rep[List[Either[Int, Int]]] = {
    val xs = FoldLeft.fromRange(a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    partitioned.apply[List[Either[Int, Int]]](
      List[Either[Int, Int]](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * partition bis map.
   */
  def partitionbismap(a: Rep[Int], b: Rep[Int]): Rep[List[Either[Int, Int]]] = {
    val xs = FoldLeft.fromRange(a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    val mapped = partitioned map { x => x.map(_ * unit(2), _ * unit(3)) }
    mapped.apply[List[Either[Int, Int]]](
      List[Either[Int, Int]](),
      (ls, x) => ls ++ List(x)
    )
  }

  /**
   * partition bis map, folded into a pair of lists
   */
  def partitionbismap2listpair(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange(a, b)
    val partitioned = (xs.partitionBis(_ % unit(2) == unit(0)))
    val mapped = partitioned map { x => x.map(_ * unit(2), _ * unit(3)) }
    mapped.apply[(List[Int], List[Int])](
      (List[Int](), List[Int]()),
      (ls, x) =>
        if (x.isLeft) (ls._1 ++ List(x.getLeft), ls._2)
        else (ls._1, ls._2 ++ List(x.getRight))
    )
  }

  /**
   * partition bis map, folded into a pair of lists
   * using EitherCPS
   */
  def partitioncpsmap2listpair(a: Rep[Int], b: Rep[Int]): Rep[(List[Int], List[Int])] = {
    val xs = FoldLeft.fromRange(a, b)
    val partitioned = (xs.partitionCPS(_ % unit(2) == unit(0)))
    val mapped = partitioned map { x => x.map(_ * unit(2), _ * unit(3)) }
    mapped.apply[(List[Int], List[Int])](
      (List[Int](), List[Int]()),
      (ls, elem) =>
        elem.apply(
          l => (ls._1 ++ List(l), ls._2),
          r => (ls._1, ls._2 ++ List(r))
        )
    )
  }

  /**
   * groupWith followed by sum
   */
  def groupwithsum(a: Rep[Int], b: Rep[Int]): Rep[HashMap[Int, Int]] = {
    val xs = FoldLeft.fromRange(a, b)
    val grouped = xs.groupWith(x => x % unit(3))

    grouped.apply[HashMap[Int, Int]](
      HashMap[Int, Int](),
      (dict, x) =>
        if (dict.contains(x._1)) { dict + (x._1, dict(x._1) + x._2) }
        else { dict + (x._1, x._2) }
    )
  }

  /**
   * reverseIndex example
   * input: a list of (name, movies) representing the movies a person likes
   * output: a map of (movie, count) representing how many people like a movie
   */
  def reverseIndex(ls: Rep[List[(String, List[String])]]): Rep[HashMap[String, Int]] = {
    val fld = FoldLeft.fromList[(String, List[String])](ls)

    val flattened: FoldLeft[(String, String)] = for {
      elem  <- fld
      movie <- FoldLeft.fromList[String](elem._2)
    } yield (elem._1, movie)

    val grouped = flattened groupWith { elem => elem._2 }
    grouped.apply[HashMap[String, Int]](
      HashMap[String, Int](),
      (dict, x) =>
        if (dict.contains(x._1)) dict + (x._1, dict(x._1) + unit(1))
        else dict + (x._1, unit(1))
    )
  }
}

trait FoldLeftGen
  extends ScalaGenListOps
  with ScalaGenIfThenElse
  with ScalaGenBooleanOps
  with ScalaGenVariables
  with ScalaGenOrderingOps
  with ScalaGenNumericOps
  with ScalaGenPrimitiveOps
  with ScalaGenWhile
  with ScalaGenEqual
  with ScalaGenEitherOps
  with MyScalaGenHashMapOps {
  val IR: FoldLeftExp
}

class FoldLeftSuite extends FileDiffSpec {

  val prefix = "test-out/"

  def `main API generate code with no diff` = {
    withOutFile(prefix + "foldleft") {
      new FoldLeftProg
          with FoldLeftExp
          with TupleOpsExp
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new FoldLeftGen with ScalaGenTupleOps { val IR: self.type = self }

        codegen.emitSource(foldLeftId _, "foldLeftId", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcfoldLeftId = compile(foldLeftId)
        scala.Console.println(testcfoldLeftId(1))
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

        codegen.emitSource2(flatMapRange _, "flatMapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapRange = compile2(flatMapRange)
        scala.Console.println(testcFlatMapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapmapRange _, "flatMapmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapmapRange = compile2(flatMapmapRange)
        scala.Console.println(testcFlatMapmapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapfiltermapRange _, "flatMapfiltermapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapfiltermapRange = compile2(flatMapfiltermapRange)
        scala.Console.println(testcFlatMapfiltermapRange(1, 5))
        codegen.reset

        codegen.emitSource2(flatMapfiltermapRange2 _, "flatMapfiltermapRange2", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcFlatMapfiltermapRange2 = compile2(flatMapfiltermapRange2)
        scala.Console.println(testcFlatMapfiltermapRange2(1, 5))
        codegen.reset

        codegen.emitSource2(mapconcatmapRange _, "mapconcatmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapconcatmapRange = compile2(mapconcatmapRange)
        scala.Console.println(testcMapconcatmapRange(1, 5))
        codegen.reset

        codegen.emitSource2(mapappendmapRange _, "mapappendmapRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMapappendmapRange = compile2(mapappendmapRange)
        scala.Console.println(testcMapappendmapRange(1, 5))
        codegen.reset
      }
    }

    assertFileEqualsCheck(prefix + "foldleft")
  }

  def `Partition generate code with no diff` = {
    withOutFile(prefix + "partition") {
      new FoldLeftProg
          with FoldLeftExp
          with TupleOpsExp
          /** this trait should be mixed in higher up */ with ArrayOpsExp
          /** this trait should be mixed in higher up */ with SeqOpsExp
          with MyScalaCompile { self =>

        val codegen = new FoldLeftGen with ScalaGenTupleOps { val IR: self.type = self }

        codegen.emitSource2(partitionmapRange _, "partitionmapRange", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionmapRange = compile2(partitionmapRange)
        scala.Console.println(testcPartitionmapRange(1, 10))
        codegen.reset

        codegen.emitSource2(multifoldleftRange _, "multifoldleftRange", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcMultifoldleftRange = compile2(multifoldleftRange)
        scala.Console.println(testcMultifoldleftRange(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbis _, "partitionbis", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbis = compile2(partitionbis)
        scala.Console.println(testcPartitionbis(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbismap _, "partitionbismap", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbismap = compile2(partitionbismap)
        scala.Console.println(testcPartitionbismap(1, 10))
        codegen.reset

        codegen.emitSource2(partitionbismap2listpair _, "partitionbismap2listpair", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitionbismap2listpair = compile2(partitionbismap2listpair)
        scala.Console.println(testcPartitionbismap2listpair(1, 10))
        codegen.reset

        codegen.emitSource2(groupwithsum _, "groupwithsum", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcGroupwithsum = compile2(groupwithsum)
        scala.Console.println(testcGroupwithsum(1, 10))
        codegen.reset

        codegen.emitSource2(partitioncpsmap2listpair _, "partitioncpsmap2listpair", new java.io.PrintWriter(System.out))
        codegen.reset

        val testcPartitioncpsmap2listpair = compile2(partitioncpsmap2listpair)
        scala.Console.println(testcPartitioncpsmap2listpair(1, 10))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "partition")
  }

  def `reverse index generate code with no diff` = {
    withOutFile(prefix + "reverse-index") {
      new FoldLeftProg
      with FoldLeftExp
      with TupleOpsExp
      with StringOpsExp
      /** this trait should be mixed in higher up */ with ArrayOpsExp
      /** this trait should be mixed in higher up */ with SeqOpsExp
      with MyScalaCompile { self =>

        val codegen = new FoldLeftGen
                      with ScalaGenTupleOps
                      with ScalaGenStringOps { val IR: self.type = self }

        codegen.emitSource(reverseIndex _, "reverseIndex", new java.io.PrintWriter(System.out))
        codegen.emitDataStructures(new java.io.PrintWriter(System.out))
        codegen.reset

      }
    }

    assertFileEqualsCheck(prefix + "reverse-index")
  }
}
