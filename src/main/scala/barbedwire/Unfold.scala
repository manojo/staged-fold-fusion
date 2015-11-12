package barbedwire

import scala.lms.common._
import lms._
import lms.util._

/**
 * Let's try to partially evaluate Iterators, or Unfolds
 */
trait Unfolds
    extends FoldLefts
    with OptionOps
    with ZeroVal {


  abstract class Iterator[A: Typ] { self =>

    type Source
    implicit def sourceTyp: Typ[Source]

    def source: Rep[Source]

    def atEnd(s: Rep[Source]): Rep[Boolean]
    def next(s: Rep[Source]): Rep[(A, Source)]

    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink: Typ](z: Rep[Sink], comb: Comb[A, Sink]): Rep[Sink] = {

        var tmpSource = source
        var tmpSink = z

        while (!atEnd(tmpSource)) {
          val elem = next(tmpSource)
          tmpSink = comb(tmpSink, elem._1)
          tmpSource = elem._2
        }

        tmpSink
      }
    }

    def map[B: Typ](f: Rep[A] => Rep[B]) = new Iterator[B] {
      type Source = self.Source
      implicit def sourceTyp = self.sourceTyp

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(B, Source)] = {
        val nextAndRest = self.next(s)
        val mapped = f(nextAndRest._1)
        make_tuple2(mapped, nextAndRest._2)
      }
    }

    /**
     * A modified filter, pumps out options
     */
    def filter(p: Rep[A] => Rep[Boolean]) = new Iterator[Option[A]] {
      type Source = self.Source
      implicit def sourceTyp = self.sourceTyp

      def source = self.source
      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(Option[A], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(if (p(nextAndRest._1)) Some(nextAndRest._1) else none[A](), nextAndRest._2)
      }
    }

    /**
     * a filter implemented like the one in the Scala standard library
     * eagerly runs the filter until it finds an element satisfying the
     * predicate
     *
     * @see `https://github.com/scala/scala/blob/2.11.x/src/library/scala/collection/Iterator.scala`
     */
    def filter2(p: Rep[A] => Rep[Boolean]): Iterator[A] = new Iterator[A] {
      type Source = self.Source
      implicit def sourceTyp = self.sourceTyp

      var hd = unit(zeroVal[A])
      var curTail = unit(zeroVal[Source])

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] =
        if (self.atEnd(s)) unit(true)
        else {
          var tmpAtEnd = unit(false)
          var tmpSource = s
          val nextAndRest = self.next(tmpSource)
          var tmpHd = nextAndRest._1
          var tmpRest = nextAndRest._2

          while (!(tmpAtEnd || p(tmpHd))) {
            tmpSource = tmpRest
            if (self.atEnd(tmpSource)) { tmpAtEnd = unit(true) }
            else {
              val nextAndRest2 = self.next(tmpSource)
              tmpHd = nextAndRest2._1
              tmpRest = nextAndRest2._2
            }
          }

          hd = tmpHd; curTail = tmpRest
          tmpAtEnd
        }

      /**
       * super effectful operation. Very ugly way to write this
       * of course, returns null values too.
       * But looks like the implementation in the standard library
       */
      def next(s: Rep[Source]): Rep[(A, Source)] =
        if (atEnd(s)) unit(zeroVal[(A, Source)])
        else make_tuple2(readVar(hd), readVar(curTail))
    }
  }

  /**
   * iterates over a list
   */
  def listIterator[T: Typ](ls: Rep[List[T]]) = new Iterator[T] {

    type Source = List[T]
    def sourceTyp: Typ[List[T]] = listTyp[T]
    def source = ls

    def atEnd(ts: Rep[List[T]]) = ts.isEmpty
    def next(ts: Rep[List[T]]) = make_tuple2(ts.head, ts.tail)

  }

  def rangeIterator(a: Rep[Int], b: Rep[Int]) = new Iterator[Int] {
    type Source = Int
    def sourceTyp = intTyp
    def source = a

    def atEnd(n: Rep[Int]): Rep[Boolean] = n > b
    def next(n: Rep[Int]) = make_tuple2(n, n + 1)

  }

}
