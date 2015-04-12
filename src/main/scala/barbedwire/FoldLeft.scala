package barbedwire

import scala.virtualization.lms.common._
import lms._
import lms.util._

/**
 * An implementation of foldr/build-like fusion
 * as a staged interpreter inside LMS
 *
 * stuff is encoded as foldLeft instead
 *
 * see the following related post: http://manojo.github.io/2015/02/19/staging-foldleft/
 *
 * the type signature of foldLeft is
 *    def foldLeft[A, B](z: B, comb: (B, A) => A)(xs: List[A]) : B
 *
 */
trait FoldLefts
    extends ListOps
    with IfThenElse
    with BooleanOps
    with Variables
    with OrderingOps
    with NumericOps
    with PrimitiveOps
    with LiftVariables
    with While
    with EitherOps
    with MyTupleOps
    with EitherCPSOps {

  /**
   * a type alias for the combination function for
   * foldLeft
   * `A` is the type of elements that pass through the fold
   * `S` is the type that is eventually computed
   */
  type Comb[A, S] = (Rep[S], Rep[A]) => Rep[S]

  /**
   * foldLeft is basically a pair of a zero value and a combination function
   */
  abstract class FoldLeft[A: Manifest] { self =>

    def apply[S: Manifest](z: Rep[S], comb: Comb[A, S]): Rep[S]

    /**
     * map
     */
    def map[B: Manifest](f: Rep[A] => Rep[B]) = new FoldLeft[B] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[B, S]) = self.apply(
          z,
          (acc: Rep[S], elem: Rep[A]) => comb(acc, f(elem))
        )
    }

    /**
     * filter
     */
    def filter(p: Rep[A] => Rep[Boolean]) = new FoldLeft[A] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[A, S]) = self.apply(
          z,
          (acc: Rep[S], elem: Rep[A]) => if (p(elem)) comb(acc, elem) else acc
        )
    }

    /**
     * flatMap
     */
    def flatMap[B: Manifest](f: Rep[A] => FoldLeft[B]) = new FoldLeft[B] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[B, S]) = self.apply(
          z,
          (acc: Rep[S], elem: Rep[A]) => {
            val nestedFld = f(elem)
            nestedFld.apply(acc, comb)
          }
        )
      }

    /**
     * concat
     */
    def concat(that: FoldLeft[A]) = new FoldLeft[A] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[A, S]) = {
        val folded: Rep[S] = self.apply(z, comb)
        that.apply(folded, comb)
      }
    }

    def ++(that: FoldLeft[A]) = this concat that

    /**
     * append
     */
    def append(elem: Rep[A]) = new FoldLeft[A] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[A, S]) = {
        val folded: Rep[S] = self.apply(z, comb)
        comb(folded, elem)
      }
    }

    def :+(elem: Rep[A]) = this append elem

    /**
     * partition
     * This will create code what will run through the original fold twice
     * once for the positive predicate, once for the negative.
     *
     * see the following related post: http://manojo.github.io/2015/03/03/staged-foldleft-partition/
     */
    def partition(p: Rep[A] => Rep[Boolean]): (FoldLeft[A], FoldLeft[A]) = {
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
    def partitionBis(p: Rep[A] => Rep[Boolean]) = new FoldLeft[Either[A, A]] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[Either[A, A], S]) = self.apply(
          z,
          (acc: Rep[S], elem: Rep[A]) =>
            if (p(elem)) comb(acc, left[A, A](elem))
            else comb(acc, right[A, A](elem))
        )
      }

    /**
     * partition, that produces a FoldLeft over `EitherCPS` instead of
     * two `FoldLeft`s. The important thing is to keep the one
     * FoldLeft abstraction. The CPS encoding is used so as to avoid creating
     * Either objects
     *
     * see http://manojo.github.io/2015/03/20/cps-encoding-either/ for more
     * details
     */
    def partitionCPS(p: Rep[A] => Rep[Boolean]): FoldLeft[EitherCPS[A, A]] =
      this map { elem =>
        if (p(elem)) mkLeft[A, A](elem) else mkRight[A, A](elem)
      }


    /**
     * groupWith
     * takes a function which computes some grouping property
     * does not create groups just yet, just propagates key-value pairs
     *
     * can be rewritten using `map`.
     * see the following related post: http://manojo.github.io/2015/03/12/staged-foldleft-groupby/
     */
    def groupWith[K: Manifest](f: Rep[A] => Rep[K]): FoldLeft[(K, A)] =
      this map (elem => make_tuple2(f(elem), elem))

  }

  /**
   * companion object, makes it easier to
   * construct folds
   */
  object FoldLeft {

    /**
     * create a fold from list
     */
    def fromList[A: Manifest](ls: Rep[List[A]]) = new FoldLeft[A] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[A, S]): Rep[S] = {
        var tmpList = ls
        var tmp = z

        while (!tmpList.isEmpty) {
          tmp = comb(tmp, tmpList.head)
          tmpList = tmpList.tail
        }

        tmp
      }
    }

    /**
     * create a fold from a range
     */
    def fromRange(a: Rep[Int], b: Rep[Int]) = new FoldLeft[Int] {
      def apply[S: Manifest](z: Rep[S], comb: Comb[Int, S]) = {
        var tmpInt = a
        var tmp = z

        while (tmpInt <= b) {
          tmp = comb(tmp, tmpInt)
          tmpInt = tmpInt + 1
        }

        tmp
      }
    }

  }
}
