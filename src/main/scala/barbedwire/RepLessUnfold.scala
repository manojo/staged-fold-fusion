package barbedwire

import scala.annotation.tailrec

/**
 * A Rep-less implementation of Unfold.
 * Also known as iterators
 */

trait RepLessUnfolds { this: RepLessFolds =>

  /**
   * Let's build a library around Unfold
   */
  abstract class Unfold[A] { self =>

    /**
     * the source type from which we pull
     */
    type Source

    /**
     * the original source (look a lot like streams in stream fusion now!)
     */
    def source: Source

    /**
     *  are we done?
     */
    def atEnd(s: Source): Boolean

    /**
     * one step
     */
    def next(s: Source): (A, Source)

    /**
     * given a source, run it to exhaustion
     * will actually just result in a Fold
     */
    def toFold: FoldLeft[A] = new FoldLeft[A] {
      def apply[Sink](z: Sink, comb: Comb[A, Sink]): Sink = {

        @tailrec
        def loop(acc: Sink, tmpSource: Source): Sink =
          if (atEnd(tmpSource)) acc
          else {
            val (elem, rest) = next(tmpSource)
            loop(comb(acc, elem), rest)
          }

        loop(z, source)
      }
    }

    /*****************
     * Usual suspects
     *****************/
    def map[B](f: A => B) = new Unfold[B] {

      type Source = self.Source

      def source = self.source

      def atEnd(s: Source) = self.atEnd(s)
      def next(s: Source) = {
        /**
         * should ideally be a `map` on the pair
         * as that shows we are simply boxing
         */
        val (elem, rest) = self.next(s)
        (f(elem), rest)
      }
    }

    /**
     * not a true flatMap cause it introduces bubbles
     */
    def flatMap[B](f: A => Unfold[B]) = new Unfold[Option[B]] {

      type Source = (self.Source, Option[Unfold[B]])

      def source = (self.source, None)

      def atEnd(s: Source) = s match {
        case (_, Some(_)) => false
        case (l, None) => self.atEnd(l)
      }

      def next(s: Source) = s match {
        case (outer, Some(u)) =>

          if (u.atEnd(u.source)) (None, (outer, None))

          else {
            val (elem, innerRest) = u.next(u.source)

            val newUnfold = new Unfold[B] {
              type Source = u.Source

              def source = innerRest
              def atEnd(s: Source) = u.atEnd(s)
              def next(s: Source) = u.next(s)
            }

            (Some(elem), (outer, Some(newUnfold)))
          }

        case (outer, None) =>
          val (nextOuter, outerRest) = self.next(outer)
          (None, (outerRest, Some(f(nextOuter))))

      }

    }

    /**
     * filter, also introduces bubbles
     */
    def filter(p: A => Boolean) = new Unfold[Option[A]] {

      type Source = self.Source

      def source = self.source

      def atEnd(s: Source) = self.atEnd(s)
      def next(s: Source) = {
        val (elem, next) = self.next(s)
        if (p(elem)) (Some(elem), next)
        else (None, next)
      }
    }

    /**
     * zip, the one that fold could not
     */
    def zip[B](that: Unfold[B]) = new Unfold[(A, B)] {
      type Source = (self.Source, that.Source)

      def source = (self.source, that.source)

      def atEnd(s: Source) = s match { case (l, r) =>
        self.atEnd(l) || that.atEnd(r)
      }

      def next(s: Source) = s match { case (l, r) =>
        val (leftElem, leftRest) = self.next(l)
        val (rightElem, rightRest) = that.next(r)

        ((leftElem, rightElem), (leftRest, rightRest))
      }
    }

    /**
     * let's do sliding
     */
//    def sliding(n: Int) = new Unfold[Unfold[A]] {
//
//      type Source = self.source
//
//      def source = self.source
//
//      def atEnd(s: Source) = self.atEnd(s)
//      def next(s: Source) =
//
//    }

//    def take(n: Int) = new Unfold[A] {
//
//      type Source = (self.Source, Int)
//
//      def source = (self.Source, n)
//
//      def atEnd(s: Source) = s match {
//        case (s2, n) => (n <= 0) || self.atEnd(s2)
//      }
//
//      def next(s: Source) = s match {
//        case (s2, n) =>
//          val (elem, rest) = self.next(s2)
//          (elem, (rest, n - 1))
//      }
//
//    }

  }

  /**
   * classic unfold
   */
  def unfold[B, A](step: B => Option[(A, B)])(seed: B): List[A] = step(seed) match {
    case None => Nil
    case Some((a, rest)) => a :: unfold(step)(rest)
  }

  /**
   * tailrec unfold
   */
  def unfold2[B, A](step: B => Option[(A, B)])(seed: B): List[A] = {
    @tailrec
    def loop(tmpB: B, tmpAcc: List[A]): List[A] = step(tmpB) match {
      case None => tmpAcc.reverse
      case Some((a, rest)) => loop(rest, a :: tmpAcc)
    }

    loop(seed, Nil)
  }

  def from(a: Int, b: Int): List[Int] = unfold[Int, Int]{ i =>
    if (i > b) None
    else Some((i, i + 1))
  }(a)

  def map[A, B](f: A => B)(ls: List[A]): List[B] = unfold[List[A], B] {
    case Nil => None
    case a :: as => Some((f(a), as))
  }(ls)

  def flatMap[A, B](f: A => List[B])(ls: List[A]): List[B] = {
    val innerList: List[Option[B]] = unfold[(List[B], List[A]), Option[B]] {
      case (Nil, Nil) => None
      case (Nil, a :: as) => Some((None, (f(a), as)))
      case (b :: bs, as) => Some((Some(b), (bs, as)))
    }((Nil, ls))

    innerList.foldRight(List[B]()){ case (elem, acc) => elem match {
      case None => acc
      case Some(b) => b :: acc
    }}
  }

}
