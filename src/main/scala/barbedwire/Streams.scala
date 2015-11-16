package barbedwire

import scala.lms.common._
import lms._
import lms.util._


/**
 * An alternate implementation of Streams as in Stream Fusion
 * With simple iterators we see that filter needs to return
 * `Iterator[Option[T]]`. This is boilerplate-y if we want
 * to follow a filter by another op, as we need to enter the inner
 * `Option`.
 *
 * Following from the Stream Fusion paper, we have an alternate encoding
 * A `Stream[A]` takes a function `next: A => Step[A, Source]`, where
 *
 *    Step[A, S] = Done | Skip(S) | Yield(A, S)
 *
 * We already split the `Done` part with `atEnd`. So we can have our `next` function
 * return Skip(S) | Yield(A, S), which we can rewrite as `(S, OptionCPS[A])`.
 * Let's see whether this generates good code too.
 *
 * Essentially we're slapping the Option monad transformer on top of `Iterator`.
 *
 * For now we ignore CPS encoding the pair, but we may need to look into that at
 * some point.
 */
trait Streams extends Unfolds {


  abstract class Stream[A: Typ] { self =>

    type Source
    implicit def sourceTyp: Typ[Source]

    def source: Rep[Source]

    def atEnd(s: Rep[Source]): Rep[Boolean]
    def next(s: Rep[Source]): Rep[(OptionCPS[A], Source)]

    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink: Typ](z: Rep[Sink], comb: Comb[A, Sink]): Rep[Sink] = {

        var tmpSource = source
        var tmpSink = z

        while (!atEnd(tmpSource)) {
          val elem = next(tmpSource)

          /**
           * The key step. This is where we peel out the option
           */
          elem._1.apply(
            _ => (),
            x => tmpSink = comb(tmpSink, x)
          )

          tmpSource = elem._2
        }

        tmpSink
      }
    }

    def map[B: Typ](f: Rep[A] => Rep[B]) = new Stream[B] {
      type Source = self.Source
      implicit def sourceTyp = self.sourceTyp

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(OptionCPS[B], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 map f, nextAndRest._2)
      }
    }

    def filter(p: Rep[A] => Rep[Boolean]) = new Stream[A] {
      type Source = self.Source
      implicit def sourceTyp = self.sourceTyp

      def source = self.source
      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(OptionCPS[A], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 filter p, nextAndRest._2)
      }
    }
  }

  /**
   * iterates over a list
   */
  def listStream[T: Typ](ls: Rep[List[T]]) = new Stream[T] {

    type Source = List[T]
    def sourceTyp: Typ[List[T]] = listTyp[T]
    def source = ls

    def atEnd(ts: Rep[List[T]]) = ts.isEmpty
    def next(ts: Rep[List[T]]) = make_tuple2(mkSome(ts.head), ts.tail)

  }

  /**
   * iterates over ranges
   */
  def rangeStream(a: Rep[Int], b: Rep[Int]) = new Stream[Int] {
    type Source = Int
    def sourceTyp = intTyp
    def source = a

    def atEnd(n: Rep[Int]): Rep[Boolean] = n > b
    def next(n: Rep[Int]) = make_tuple2(mkSome(n), n + 1)

  }

}
