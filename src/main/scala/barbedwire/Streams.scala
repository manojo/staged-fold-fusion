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
 *
 * see the following related post: http://manojo.github.io/2016/01/13/stream-fusion-is-pefible/
 *
 */
trait Streams
    extends Unfolds
    with PairCPS {

  import PairCPS._
  import OptionCPS._

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def stream_typ[A: Typ, S: Typ]: Typ[Stream[A, S]]
  implicit def stream_nul[A: Typ: Nul, S: Typ: Nul]: Nul[Stream[A, S]]

  abstract class Stream[A: Typ: Nul, Source: Typ: Nul] { self =>

    def source: Rep[Source]

    def atEnd(s: Rep[Source]): Rep[Boolean]
    def next(s: Rep[Source]): Rep[PairCPS[OptionCPS[A], Source]]

    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink: Typ: Nul](z: Rep[Sink], comb: Comb[A, Sink]): Rep[Sink] = {

        var tmpSource = source
        var tmpSink = z

        while (!atEnd(tmpSource)) {

          var optA = zeroVal[OptionCPS[A]]

          /**
           * peeling out the pair and the option
           */
          next(readVar(tmpSource)).apply { (optcps, src) =>
            optA = optcps
            tmpSource = src
          }

          readVar(optA).apply(
            _ => unit(()),
            e => tmpSink = comb(tmpSink, e)
          )
        }

        tmpSink
      }
    }

    def map[B: Typ: Nul](f: Rep[A] => Rep[B]) = new Stream[B, Source] {

      def source = self.source

      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[PairCPS[OptionCPS[B], Source]] = {
        val nextAndRest = self.next(s)
        nextAndRest.map(
          nxt => nxt map f,
          rst => rst
        )
      }
    }

    def filter(p: Rep[A] => Rep[Boolean]) = new Stream[A, Source] {

      def source = self.source
      def atEnd(s: Rep[Source]): Rep[Boolean] = self.atEnd(s)
      def next(s: Rep[Source]): Rep[PairCPS[OptionCPS[A], Source]] = {
        val nextAndRest = self.next(s)
        nextAndRest.map(
          nxt => nxt filter p,
          rst => rst
        )
      }
    }

    /**
     * The zip function, not completely trivial in fact!
     * We need state for:
     *  - knowing whether we need to pull from the left, or from the right
     *  - capturing the current states of the left or right sources
     *
     * With no Rep or CPS types, that is (Option[A], (S1, S2))
     */
    def zip[B: Typ: Nul, S2: Typ: Nul](that: Stream[B, S2]) = {

      type InnerSource = PairCPS[OptionCPS[A], PairCPS[Source, S2]]

      /**
       * outright voodoo. In the absence of this implicit the
       * type checker gives a "diverging implicit exception"
       */
      implicit val p_typ = paircps_typ[Source, S2]
      implicit val p_nul = paircps_nul[Source, S2]

      new Stream[PairCPS[A, B], PairCPS[OptionCPS[A], PairCPS[Source, S2]]] { zipped =>

        def source = mkPair(mkNone[A], mkPair(self.source, that.source))

        /**
         * we allow for one list to be longer than the other one
         */
        def atEnd(p: Rep[InnerSource]): Rep[Boolean] = {
          p.apply { (optA, sources) => sources.apply { (s1, s2) =>
            (self.atEnd(s1) && !optA.isDefined) || that.atEnd(s2)
          }}
        }

        /**
         * a zip is a join point, hence the need for "materializing" the option
         */
        def next(s: Rep[InnerSource]): Rep[PairCPS[OptionCPS[PairCPS[A, B]], InnerSource]] = {

          /**
           * Alas, these mutable variables are declared here
           * because I get diverging implicit errors if I try to
           * return pairs at each branch.
           * Too confusing to figure out the voodoo there
           */
          var tmpOptA = mkNone[A]
          var tmpOptAB = mkNone[PairCPS[A, B]]

          var tmpS1 = zeroVal[Source]
          var tmpS2 = zeroVal[S2]


          s.apply { (optA, sources) => sources.apply { (s1, s2) =>

            tmpOptA = optA; tmpS1 = s1; tmpS2 = s2

            optA.apply(
              /** if `optA` not defined we pull from `s1` */
              _ => {
                val nextAandRest = self.next(s1)
                nextAandRest.apply { (newOptA, s1Rest) =>
                  tmpOptA = newOptA
                  tmpS1 = s1Rest
                }
              },

              /** if `optA` is defined we pull from `s2` */
              a => {
                val nextBandRest = that.next(s2)
                nextBandRest.apply{ (optB, s2Rest) =>
                  optB.apply(

                    /** if `optB` not defined we continue */
                    _ => { tmpS2 = s2Rest },

                    /** if `optB` is defined we can create a pair */
                    b => {
                      tmpS2 = s2Rest
                      tmpOptAB = mkSome(mkPair(a, b))
                      tmpOptA = mkNone[A]
                    }
                  )
                }
              }
            )
          }}

          mkPair(readVar(tmpOptAB), mkPair(readVar(tmpOptA), mkPair(readVar(tmpS1), readVar(tmpS2))))
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
    def flatten[B: Typ: Nul, S2: Typ: Nul](
      init: Rep[A] => Rep[S2],
      atEndInner: Rep[S2] => Rep[Boolean],
      nextInner: Rep[S2] => Rep[PairCPS[OptionCPS[B], S2]]
    ) = new Stream[B, PairCPS[Source, OptionCPS[S2]]] {

      /** type alias for simplifying notation */
      type TotalSource = PairCPS[Source, OptionCPS[S2]]
      implicit def sourceTyp = tuple2_typ[Source, OptionCPS[S2]]
      implicit def sourceNul = tuple2_nul[Source, OptionCPS[S2]]

      def source = mkPair(self.source, mkNone[S2])

      def atEnd(s: Rep[TotalSource]): Rep[Boolean] = s.apply { (outer, inner) =>
        self.atEnd(outer) && !(inner.isDefined)
      }

      def next(s: Rep[TotalSource]): Rep[PairCPS[OptionCPS[B], TotalSource]] = {

        var tmpOptB = mkNone[B]
        var tmpOuter = zeroVal[Source]
        var tmpInner = mkNone[S2]

        s.apply { (outer, optInner) =>

          tmpOuter = outer
          tmpInner = optInner

          optInner.apply(
            /** if no inner source, we pull from the outer one */
            _ =>  {
              val nextAndRest = self.next(outer)
              nextAndRest.apply((optA, rest) => {
                tmpInner = optA map init
                tmpOuter = rest
              })
            },
            /**  if inner source, we pull from it */
            inner => {
              if (atEndInner(inner)) {
                tmpInner = mkNone[S2]
              } else {
                val nextAndRest = nextInner(inner)
                nextAndRest.apply((optB, rest) =>  {
                  tmpOptB = optB
                  tmpInner = mkSome(rest)
                })
              }
            }
          )
        }

        mkPair(readVar(tmpOptB), mkPair(tmpOuter, tmpInner))

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
    def flatMap[B: Typ: Nul, S2: Typ: Nul]
               (f: Rep[A] => Stream[B, S2]): Stream[B, PairCPS[Source, OptionCPS[PairCPS[A, S2]]]] = {

      implicit def pairTyp = paircps_typ[A, S2]
      implicit def pairNul = paircps_nul[A, S2]

      implicit def sourceTyp = paircps_typ[Source, OptionCPS[PairCPS[A, S2]]]
      implicit def sourceNul = paircps_nul[Source, OptionCPS[PairCPS[A, S2]]]

      new Stream[B, PairCPS[Source, OptionCPS[PairCPS[A, S2]]]] {

        /** type alias for simplifying notation */
        type TotalSource = PairCPS[Source, OptionCPS[PairCPS[A, S2]]]

        def source = mkPair(self.source, mkNone[PairCPS[A, S2]])

        def atEnd(s: Rep[TotalSource]): Rep[Boolean] = s.apply { (outer, inner) =>
          self.atEnd(outer) && !(inner.isDefined)
        }

        def next(s: Rep[TotalSource]): Rep[PairCPS[OptionCPS[B], TotalSource]] = {

          var tmpOptB = mkNone[B]
          var tmpOuter = zeroVal[Source]
          var tmpInner = mkNone[PairCPS[A, S2]]

          s.apply { (outer, optInner) =>

            tmpOuter = outer; tmpInner = optInner

            optInner.apply(
              /** if no inner source, pull from outer */
              _ => {
                val nextAndRest = self.next(outer)

                nextAndRest.apply { (optA, rest) =>
                  tmpOuter = rest
                  tmpInner = optA map (a => mkPair(a, f(a).source))
                }
              },

              /**
               * if the inner source is defined we pull from it
               */
              inner => inner.apply { (innerA, innerSource) =>

                /** calling the innerStream, to have access to the functions */
                val innerStream = f(innerA)

                if (innerStream.atEnd(innerSource)) {
                  tmpInner = mkNone[PairCPS[A, S2]]
                } else {
                  val nextAndRest = innerStream.next(innerSource)
                  nextAndRest.apply { (optB, rest) =>
                    tmpOptB = optB
                    tmpInner = mkSome(mkPair(innerA, rest))
                  }
                }
              }
            )
          }

          mkPair(readVar(tmpOptB), mkPair(readVar(tmpOuter), readVar(tmpInner)))

        }
      }
    }
  }
  /**
   * iterates over a list
   */
  def listStream[T: Typ: Nul](ls: Rep[List[T]]) = new Stream[T, List[T]] {

    def source = ls

    def atEnd(ts: Rep[List[T]]) = ts.isEmpty
    def next(ts: Rep[List[T]]) = mkPair(mkSome(ts.head), ts.tail)

  }

  /**
   * iterates over ranges
   */
  def rangeStream(a: Rep[Int], b: Rep[Int]) = new Stream[Int, Int] {

    def source = a

    def atEnd(n: Rep[Int]): Rep[Boolean] = n > b
    def next(n: Rep[Int]) = mkPair(mkSome(n), n + 1)

  }

  /**
   * wrappers delight!
   * We want to represent a `Rep[Stream]`
   */
/*  implicit class StreamOpsCls[A: Typ: Nul, S: Typ: Nul](str: Rep[Stream[A, S]]) {

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


  def stream_atEnd[A: Typ: Nul, S: Typ: Nul](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Boolean]

  def stream_next[A: Typ: Nul, S: Typ: Nul](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[(Option[A], S)]

  def stream_source[A: Typ: Nul, S: Typ: Nul](
    str: Rep[Stream[A, S]]
  ): Rep[S]

  def stream_addSource[A: Typ: Nul, S: Typ: Nul](
    str: Rep[Stream[A, S]],
    s: Rep[S]
  ): Rep[Stream[A, S]]

  def mkStream[A: Typ: Nul, S: Typ: Nul](str: Stream[A, S]): Rep[Stream[A, S]]
  def stream_tofold[A: Typ: Nul, S: Typ: Nul](str: Rep[Stream[A, S]]): FoldLeft[A]

  /**
   * tests on alternate signatures
   * ignoring for now.
   */
  //def stream_atBla[A: Typ: Nul](str: Rep[Stream[A]]{ type Src })(s: str.Src): Rep[Boolean]
*/
}


/**
 * The full wrapper in delight
 */
trait StreamExp
    extends Streams
    with UnfoldExp
    with PairCPSExp {

  /**
   * implicits for creating Type Manifests
   * new boilerplate after the Manifest -> Typ change
   */
  implicit def stream_typ[A: Typ, S: Typ]: Typ[Stream[A, S]] = {
    implicit val ManifestTyp(mA) = typ[A]
    implicit val ManifestTyp(mS) = typ[S]
    manifestTyp
  }

  implicit def stream_nul[A: Typ: Nul, S: Typ: Nul] = new Nul[Stream[A, S]] {
    def nullValue = null
    def nlArguments = nul[A] :: nul[S] :: Nil
  }

//  def mkStream[A: Typ: Nul, S: Typ: Nul](str: Stream[A, S]): Rep[Stream[A, S]] = unit(str)
//
//  def stream_atEnd[A: Typ: Nul, S: Typ: Nul](
//    str: Rep[Stream[A, S]],
//    s: Rep[S]
//  ): Rep[Boolean] = str match {
//    case Const(innerStr) => innerStr.atEnd(s)
//  }
//
//  def stream_next[A: Typ: Nul, S: Typ: Nul](
//    str: Rep[Stream[A, S]],
//    s: Rep[S]
//  ): Rep[(Option[A], S)] = str match {
//    case Const(innerStr) => innerStr.next(s)
//  }
//
//  def stream_source[A: Typ: Nul, S: Typ: Nul](
//    str: Rep[Stream[A, S]]
//  ): Rep[S] = str match {
//    case Const(innerStr) => innerStr.source
//  }
//
//  def stream_addSource[A: Typ: Nul, S: Typ: Nul](
//    str: Rep[Stream[A, S]],
//    s: Rep[S]
//  ): Rep[Stream[A, S]] = str match {
//    case Const(innerStr) => unit(innerStr.addNewSource(s))
//  }
//
//  def stream_tofold[A: Typ: Nul, S: Typ: Nul](str: Rep[Stream[A, S]]): FoldLeft[A] = str match {
//    case Const(innerStr) => innerStr.toFold
//  }
//
}
