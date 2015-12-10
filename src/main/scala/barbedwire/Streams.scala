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
    def next(s: Rep[Source]): Rep[(Option[A], Source)]

    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink: Typ](z: Rep[Sink], comb: Comb[A, Sink]): Rep[Sink] = {

        var tmpSource = source
        var tmpSink = z

        while (!atEnd(tmpSource)) {
          val elem: Rep[(Option[A], Source)] = next(tmpSource)

          /**
           * The key step. This is where we peel out the option
           */
          //elem._1.apply(
          //  _ => (),
          //  x => tmpSink = comb(tmpSink, x)
          //)
          if (elem._1.isDefined) {
            tmpSink = comb(tmpSink, elem._1.get)
          }

          tmpSource = elem._2
        }

        tmpSink
      }
    }

    def map[B: Typ](f: Rep[A] => Rep[B]) = new Stream[B, Source] {

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(Option[B], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 map f, nextAndRest._2)
      }
    }

    def filter(p: Rep[A] => Rep[Boolean]) = new Stream[A, Source] {

      def source = self.source
      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[(Option[A], Source)] = {
        val nextAndRest = self.next(s)
        make_tuple2(nextAndRest._1 filter p, nextAndRest._2)
      }
    }

    def zip[B: Typ, S2: Typ](that: Stream[B, S2]) = new Stream[(A, B), (Source, S2)] {

      type InnerSource = (Source, S2)
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
        //var leftIsDefined = unit(false); var leftElem = unit(zeroVal[A])
        //var rightIsDefined = unit(false); var rightElem = unit(zeroVal[B])

        val leftNext = self.next(s._1)
        val leftIsDefined = leftNext._1.isDefined
        val leftElem = leftNext._1.get
        //leftNext._1.apply(
        //  _ => (),
        //  elem => { leftIsDefined = unit(true); leftElem = elem }
        //)

        val rightNext = that.next(s._2)
        val rightIsDefined = rightNext._1.isDefined
        val rightElem = rightNext._1.get
        //rightNext._1.apply(
        //  _ => (),
        //  elem => { rightIsDefined = unit(true); rightElem = elem }
        //)

        if (leftIsDefined) {
          if (rightIsDefined) {
            /**
              * need to explicitly type this because implicits
              * fail to kick in otherwise
              */
            val bla: Rep[(Option[(A, B)], InnerSource)] = make_tuple2(
              /*mk*/Some(make_tuple2(leftElem, rightElem)),
              make_tuple2(leftNext._2, rightNext._2)
            )
            bla

          } else make_tuple2(
            /*mkN*/none[(A, B)](),
            make_tuple2(s._1, rightNext._2)
          )
        } else {
          if (rightIsDefined) make_tuple2(
            /*mkN*/none[(A, B)](),
            make_tuple2(leftNext._2, s._2)
          ) else make_tuple2(
            /*mkN*/none[(A, B)](),
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

    /**
     * A simpler version of `flatMap`, called `flatten`. In this
     * version, the `next` and `atEnd` functions must be static for
     * all inner streams.
     *
     * @see HERMIT in the Stream, by Farmer et al., Coutts thesis, for
     * more details
     */
    def flatten[B: Typ, S2: Typ](
      init: Rep[A] => Rep[S2],
      atEndInner: Rep[S2] => Rep[Boolean],
      nextInner: Rep[S2] => Rep[(Option[B], S2)]
    ): Stream[B, (Source, Option[S2])] = new Stream[B, (Source, Option[S2])] {

      /** type alias for simplifying notation */
      type TotalSource = (Source, Option[S2])
      implicit def sourceTyp = tuple2_typ[Source, Option[S2]]

      def source = (self.source, none[S2]())

      def atEnd(s: Rep[TotalSource]): Rep[Boolean] = {
        val outer = s._1
        val inner = s._2

        self.atEnd(outer) && !(inner.isDefined)
      }

      def next(s: Rep[TotalSource]): Rep[(Option[B], TotalSource)] = {
        val outer = s._1
        val innerMaybe: Rep[Option[S2]] = s._2

        /** pull from the inner source */
        if (innerMaybe.isDefined) {

          if (atEndInner(innerMaybe.get)) {
           /**
            * need to explicitly type this because implicits
            * fail to kick in otherwise
            */
            val bla: Rep[(Option[B], TotalSource)] =
              make_tuple2(/*mkN*/none[B](), (outer, none[S2]()))
            bla
          } else {
            val nextAndRest = nextInner(innerMaybe.get)
            val nextB: Rep[Option[B]] = nextAndRest._1
            val rest: Rep[S2] = nextAndRest._2
            /**
             * need to explicitly type this because implicits
             * fail to kick in otherwise
             */
            //val newOptB: Rep[Option[B]] = option_conditional(nextB.isDefined, mkSome(nextB.get), mkNone[B])

            val bla: Rep[(Option[B], TotalSource)] =
              make_tuple2(nextB /*newOptB*/, (outer, make_opt(Some(rest))))

            bla
          }
        } else {
          /** pull from outer source */
          val nextAndRest = self.next(outer)
          val nextA: Rep[Option[A]] = nextAndRest._1
          val rest: Rep[Source] = nextAndRest._2

          /**
           * need to explicitly type this because implicits
           * fail to kick in otherwise
           */
          val bla: Rep[(Option[B], TotalSource)] = {
            val newS2: Rep[Option[S2]] = (nextA map init)//.toOption
            make_tuple2(/*mkN*/none[B](), (rest, /*none[S2]()*/newS2))
          }

          bla
        }
      }
    }

    /**
     * The big one, flatMap
     * We know that the function passed to `flatMap` is static
     * so we just want to keep the source and the current element
     * of the outer stream in the state.
     *
     * This follows the "name-capturing" `flatten` by Farmer et al.
     * in the HERMIT in the stream paper.
     */
    def flatMap[B: Typ, S2: Typ](f: Rep[A] => Stream[B, S2]): Stream[B, (Source, Option[(A, S2)])] = {
      implicit def pairTyp = tuple2_typ[A, S2]
      implicit def sourceTyp = tuple2_typ[Source, Option[(A, S2)]]

      new Stream[B, (Source, Option[(A, S2)])] {

        /** type alias for simplifying notation */
        type TotalSource = (Source, Option[(A, S2)])


        def source = (self.source, none[(A, S2)]())

        def atEnd(s: Rep[TotalSource]): Rep[Boolean] = {
          val outer = s._1
          val inner = s._2

          self.atEnd(outer) && !(inner.isDefined)
        }

        def next(s: Rep[TotalSource]): Rep[(Option[B], TotalSource)] = {
          val outer = s._1
          val innerMaybe = s._2

          /** pull from the inner source */
          if (innerMaybe.isDefined) {
            val innerA: Rep[A] = innerMaybe.get._1
            val innerSource: Rep[S2] = innerMaybe.get._2

            /** calling the innerStream, to have access to the functions */
            val innerStream = f(innerA)

            if (innerStream.atEnd(innerSource)) {
             /**
              * need to explicitly type this because implicits
              * fail to kick in otherwise
              */
              val bla: Rep[(Option[B], TotalSource)] =
                make_tuple2(/*mkN*/none[B](), (outer, none[(A, S2)]()))
              bla
            } else {
              val nextAndRest = innerStream.next(innerSource)
              /**
               * need to explicitly type this because implicits
               * fail to kick in otherwise
               */
               val bla: Rep[(Option[B], TotalSource)] =
                 make_tuple2(nextAndRest._1, (outer, make_opt(Some(make_tuple2(innerA, nextAndRest._2)))))
               bla
            }
          } else {
            /** pull from outer source */
            val nextAndRest = self.next(outer)
            val nextMaybe = nextAndRest._1
            val rest = nextAndRest._2

            /**
             * need to explicitly type this because implicits
             * fail to kick in otherwise
             */
             val bla: Rep[(Option[B], TotalSource)] = {
              val newS2: Rep[Option[(A, S2)]] = (nextMaybe map { a => make_tuple2(a, f(a).source) })//.toOption
              make_tuple2(/*mkN*/none[B](), (rest, newS2))
             }

             bla
          }
        }
      }

    }
  }

  /**
   * iterates over a list
   */
  def listStream[T: Typ](ls: Rep[List[T]]) = new Stream[T, List[T]] {

    def source = ls

    def atEnd(ts: Rep[List[T]]) = ts.isEmpty
    def next(ts: Rep[List[T]]) = make_tuple2(/*mk*/Some(ts.head), ts.tail)

  }

  /**
   * iterates over ranges
   */
  def rangeStream(a: Rep[Int], b: Rep[Int]) = new Stream[Int, Int] {

    def source = a

    def atEnd(n: Rep[Int]): Rep[Boolean] = n > b
    def next(n: Rep[Int]) = make_tuple2(/*mk*/Some(n), n + 1)

  }

  /**
   * wrappers delight!
   * We want to represent a `Rep[Stream]`
   */
  implicit class StreamOpsCls[A: Typ, S: Typ](str: Rep[Stream[A, S]]) {

    def next(s: Rep[S]): Rep[(Option[A], S)] = stream_next(str, s)
    def atEnd(s: Rep[S]): Rep[Boolean] = stream_atEnd(str, s)
    def source: Rep[S] = stream_source(str)
    def addNewSource(s: Rep[S]) = stream_addSource(str, s)

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
  ): Rep[(Option[A], S)]

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
trait StreamExp
    extends Streams
    with UnfoldExp
    /*with StructExp*/ {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def stream_typ[A: Typ, S: Typ]: Typ[Stream[A, S]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mS) = typ[S]
    manifestTyp
  }

  def mkStream[A: Typ, S: Typ](str: Stream[A, S]): Rep[Stream[A, S]] = unit(str)

  def stream_atEnd[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Boolean] = str match {
    case Const(innerStr) => innerStr.atEnd(s)
  }

  def stream_next[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[(Option[A], S)] = str match {
    case Const(innerStr) => innerStr.next(s)
  }

  def stream_source[A: Typ, S: Typ](
    str: Rep[Stream[A, S]]
  ): Rep[S] = str match {
    case Const(innerStr) => innerStr.source
  }

  def stream_addSource[A: Typ, S: Typ](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Stream[A, S]] = str match {
    case Const(innerStr) => unit(innerStr.addNewSource(s))
  }

  def stream_tofold[A: Typ, S: Typ](str: Rep[Stream[A, S]]): FoldLeft[A] = str match {
    case Const(innerStr) => innerStr.toFold
  }

}

