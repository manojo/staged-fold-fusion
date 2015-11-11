package barbedwire

import scala.annotation.tailrec

/**
 * A Rep-less implementation of FoldLeft
 * Easier to run functionality tests on.
 *
 * A copy-paste of `FoldLeft.scala`.
 */

trait RepLessFolds {

  /**
   * a type alias for the combination function for
   * foldLeft
   * `A` is the type of elements that pass through the fold
   * `S` is the type that is eventually computed
   */
  type Comb[A, S] = (S, A) => S

  /**
   * foldLeft is basically a pair of a zero value and a combination function
   */
  abstract class FoldLeft[A] { self =>

    def apply[S](z: S, comb: Comb[A, S]): S

    /**
     * map
     */
    def map[B](f: A => B) = new FoldLeft[B] {
      def apply[S](z: S, comb: Comb[B, S]) = self.apply(
        z,
        (acc: S, elem: A) => comb(acc, f(elem))
      )
    }

    /**
     * filter
     */
    def filter(p: A => Boolean) = new FoldLeft[A] {
      def apply[S](z: S, comb: Comb[A, S]) = self.apply(
        z,
        (acc: S, elem: A) => if (p(elem)) comb(acc, elem) else acc
      )
    }

    /**
     * flatMap
     */
    def flatMap[B](f: A => FoldLeft[B]) = new FoldLeft[B] {
      def apply[S](z: S, comb: Comb[B, S]) = self.apply(
        z,
        (acc: S, elem: A) => {
          val nestedFld = f(elem)
          nestedFld.apply(acc, comb)
        }
      )
    }

    /**
     * concat
     */
    def concat(that: FoldLeft[A]) = new FoldLeft[A] {
      def apply[S](z: S, comb: Comb[A, S]) = {
        val folded: S = self.apply(z, comb)
        that.apply(folded, comb)
      }
    }

    def ++(that: FoldLeft[A]) = this concat that

    /**
     * append
     */
    def append(elem: A) = new FoldLeft[A] {
      def apply[S](z: S, comb: Comb[A, S]) = {
        val folded: S = self.apply(z, comb)
        comb(folded, elem)
      }
    }

    def :+(elem: A) = this append elem

    /**
     * partition
     * This will create code what will run through the original fold twice
     * once for the positive predicate, once for the negative.
     *
     * see the following related post: http://manojo.github.io/2015/03/03/staged-foldleft-partition/
     */
    def partition(p: A => Boolean): (FoldLeft[A], FoldLeft[A]) = {
      val trues = this filter p
      val falses = this filter (a => !p(a))
      (trues, falses)
    }

    /**
     * partition, that produces a FoldLeft over `Either` instead of
     * two `FoldLeft`s. The important thing is to keep the one
     * FoldLeft abstraction.
     * This can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def partitionBis(p: A => Boolean) = new FoldLeft[Either[A, A]] {
      def apply[S](z: S, comb: Comb[Either[A, A], S]) = self.apply(
        z,
        (acc: S, elem: A) =>
          if (p(elem)) comb(acc, Left(elem))
          else comb(acc, Right(elem))
      )
    }


    /**
     * groupWith
     * takes a function which computes some grouping property
     * does not create groups just yet, just propagates key-value pairs
     *
     * can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def groupWith[K](f: A => K): FoldLeft[(K, A)] =
      this map (elem => (f(elem), elem))

  }

  /**
   * companion object, makes it easier to
   * construct folds
   */
  object FoldLeft {

    /**
     * create a fold from list
     */
    def fromList[A](ls: List[A]) = new FoldLeft[A] {
      def apply[S](z: S, comb: Comb[A, S]): S = {

        @tailrec
        def loop(acc: S, rest: List[A]): S = rest match {
          case Nil => acc
          case x :: xs => loop(comb(acc, x), xs)
        }

        loop(z, ls)
      }
    }

    /**
     * create a fold from a range
     */
    def fromRange(a: Int, b: Int) = new FoldLeft[Int] {
      def apply[S](z: S, comb: Comb[Int, S]) = {

        @tailrec
        def loop(acc: S, curElem: Int): S = {
          if (curElem > b) acc
          else loop(comb(acc, curElem), curElem + 1)
        }

        loop(z, a)
      }
    }

  }


}
