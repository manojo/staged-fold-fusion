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

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def stream_typ[A: Typ, S: Typ]: Typ[Stream[A, S]]

  abstract class Stream[A: Typ, Source: Typ] { self =>

    def source: Rep[Source]

    def atEnd(s: Rep[Source]): Rep[Boolean]
    def next(s: Rep[Source]): Rep[(OptionCPS[A], Source)]

    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink: Typ](z: Rep[Sink], comb: Comb[A, Sink]): Rep[Sink] = {

        var tmpSource = source
        var tmpSink = z

        while (!atEnd(tmpSource)) {
          val elem: Rep[(OptionCPS[A], Source)] = next(tmpSource)

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

    def map[B: Typ](f: Rep[A] => Rep[B]) = new Stream[B, Source] {

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(OptionCPS[B], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 map f, nextAndRest._2)
      }
    }

    def filter(p: Rep[A] => Rep[Boolean]) = new Stream[A, Source] {

      def source = self.source
      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(OptionCPS[A], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 filter p, nextAndRest._2)
      }
    }

    def zip[B: Typ, S2: Typ](that: Stream[B, S2]) = new Stream[(A, B), (Source, S2)] {

      type InnerSource = (Source, S2)
//      implicit def slfSourceTyp: Typ[Source] = typ[Source]
//      implicit def thatSourceTyp = typ[S2]
      implicit def sourceTyp = tuple2_typ[Source, S2]

      /**
       * outright voodoo. In the absence of this implicit the
       * type checker gives a "diverging implicit exception"
       */
      implicit val typAB = tuple2_typ[A, B]


      def source = make_tuple2(self.source, that.source)

      /**
       * we allow for one list to be longer than the other one
       */
      def atEnd(s: Rep[InnerSource]) = self.atEnd(s._1) || that.atEnd(s._2)

      /**
       * a zip is a join point, hence the need for "materializing" the option
       */
      def next(s: Rep[InnerSource]) = {
        var leftIsDefined = unit(false); var leftElem = unit(zeroVal[A])
        var rightIsDefined = unit(false); var rightElem = unit(zeroVal[B])

        val leftNext = self.next(s._1)
        leftNext._1.apply(
          _ => (),
          elem => { leftIsDefined = unit(true); leftElem = elem }
        )

        val rightNext = that.next(s._2)
        rightNext._1.apply(
          _ => (),
          elem => { rightIsDefined = unit(true); rightElem = elem }
        )

        if (leftIsDefined) {
          if (rightIsDefined) {
            /**
              * need to explicitly type this because implicits
              * fail to kick in otherwise
              */
            val bla: Rep[(OptionCPS[(A, B)], InnerSource)] = make_tuple2(
              mkSome(make_tuple2(leftElem, rightElem)),
              make_tuple2(leftNext._2, rightNext._2)
            )
            bla

          } else make_tuple2(
            mkNone[(A, B)],
            make_tuple2(s._1, rightNext._2)
          )
        } else {
          if (rightIsDefined) make_tuple2(
            mkNone[(A, B)],
            make_tuple2(leftNext._2, s._2)
          ) else make_tuple2(
            mkNone[(A, B)],
            make_tuple2(leftNext._2, rightNext._2)
          )
        }
      }
    }

    /**
     * A helper for creating a new Stream with a different source
     */
    def addNewSource(s: Rep[Source]) = new Stream[A, Source] {
      def source = s
      def next(s: Rep[Source]) = self.next(s)
      def atEnd(s: Rep[Source]) = self.atEnd(s)
    }

/*
    /**
     * The big one, flatMap
     */
    def flatMap[B: Typ, S2: Typ](f: Rep[A] => Stream[B, S2]): Stream[B, (Source, Option[Stream[B, S2]])] = new Stream[B, (Source, Option[Stream[B, S2]])] {

      /**
       * this encoding is in tune with the Stream Fusion one
       * We directly use `Option` and not `OptionCPS` because
       * we need a materialized result. To be determined whether
       * the boxing here results in bad perf
       */
      type S = (Source, Option[Stream[B, S2]])

      /**
       * need a sourceTyp etc.
       */
      //implicit def streamTyp =
      implicit def slfSourceTyp: Typ[self.Source] = self.sourceTyp
      implicit def itTyp: Typ[Stream[B]] = ???
      implicit def sourceTyp = tuple2_typ[self.Source, Option[Stream[B]]]

      def source = make_tuple2(self.source,  none[Stream[B, S2]])
      def atEnd(s: Rep[Source]) = self.atEnd(s._1) && !(s._2.isDefined)

      def next(s: Rep[Source]): Rep[(OptionCPS[B], Source)] = {

        val outer = s._1
        val innerOpt = s._2

        /**
         * is `innerOpt` is around, then we pull from this guy
         */
        if (innerOpt.isDefined) {
          val inner: Rep[Stream[B, S2]] = innerOpt.get
          val elem = stream_next(inner, stream_source(inner))
          val newStream = stream_addSource(inner, elem._2)
          make_tuple2(
            elem._1,
            make_tuple2(outer, Some(newStream))
          )
        } else {

          /**
           * otherwise we pull from the left side
           */
          //val elem: Rep[(OptionCPS[A], self.Source)] = self.next(outer)
          //val newInnerIt: Option[Stream[B]] = elem flatMap { a => f(a) }
          //make_tuple2(mkNone[B], make_tuple2(elem._2, newInnerIt))
        }
      }

    }
    */
  }

  /**
   * iterates over a list
   */
  def listStream[T: Typ](ls: Rep[List[T]]) = new Stream[T, List[T]] {

    def source = ls

    def atEnd(ts: Rep[List[T]]) = ts.isEmpty
    def next(ts: Rep[List[T]]) = make_tuple2(mkSome(ts.head), ts.tail)

  }

  /**
   * iterates over ranges
   */
  def rangeStream(a: Rep[Int], b: Rep[Int]) = new Stream[Int, Int] {

    def source = a

    def atEnd(n: Rep[Int]): Rep[Boolean] = n > b
    def next(n: Rep[Int]) = make_tuple2(mkSome(n), n + 1)

  }

  /**
   * wrappers delight!
   * We want to represent a `Rep[Stream]`
   */
  implicit class StreamOpsCls[A: Typ, S: Typ](str: Rep[Stream[A, S]]) {

    def next(s: Rep[S]): Rep[(OptionCPS[A], S)] = stream_next(str, s)
    def atEnd(s: Rep[S]): Rep[Boolean] = stream_atEnd(str, s)
    //def map[B: Typ](f: Rep[A] => Rep[B]) = stream_map(str, f)

    def toFold: FoldLeft[A] = stream_tofold(str)

    /**
     * we won't define some operations on `Rep[Stream]`, such as `toFold`.
     * we are only interested in folding from the outside, at least for now.
     * same for `zip`.
     * other ops, we don't define out of laziness
     */
  }


  def stream_atEnd[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Boolean]

  def stream_next[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[(OptionCPS[A], S)]

  def stream_source[A: Typ, S: Typ](
    str: Rep[Stream[A, S]]
  ): Rep[S]

  def stream_addSource[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Stream[A, S]]

  def mkStream[A: Typ, S: Typ](str: Stream[A, S]): Rep[Stream[A, S]]
  def stream_tofold[A: Typ, S: Typ](str: Rep[Stream[A, S]]): FoldLeft[A]

  /**
   * tests on alternate signatures
   * ignoring for now.
   */
  //def stream_atBla[A: Typ](str: Rep[Stream[A]]{ type Src })(s: str.Src): Rep[Boolean]

}


/**
 * The full wrapper in delight
 */
trait StreamExp extends Streams with UnfoldExp {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def stream_typ[A: Typ, S: Typ]: Typ[Stream[A, S]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mS) = typ[S]
    manifestTyp
  }

  case class StreamWrapper[A: Typ, S: Typ](str: Stream[A, S])
    extends Def[Stream[A, S]]

  def stream_atEnd[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Boolean] = str match {
    case Def(StreamWrapper(innerStr)) => innerStr.atEnd(s)
  }

  def stream_next[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[(OptionCPS[A], S)] = str match {
    case Def(StreamWrapper(innerStr)) => innerStr.next(s)
  }

  def stream_source[A: Typ, S: Typ](
    str: Rep[Stream[A, S]]
  ): Rep[S] = str match {
    case Def(StreamWrapper(innerStr)) => innerStr.source
  }

  def stream_addSource[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Stream[A, S]] = str match {
    case Def(StreamWrapper(innerStr)) => StreamWrapper(innerStr.addNewSource(s))
  }

  def mkStream[A: Typ, S: Typ](str: Stream[A, S]): Rep[Stream[A, S]] =
    StreamWrapper(str)

  def stream_tofold[A: Typ, S: Typ](str: Rep[Stream[A, S]]): FoldLeft[A] = str match {
    case Def(StreamWrapper(innerStr)) => innerStr.toFold
  }

}

