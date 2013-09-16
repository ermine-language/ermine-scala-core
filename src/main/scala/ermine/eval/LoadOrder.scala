package ermine.eval

import scalaz._
import std.map._, std.set._, std.anyVal._, syntax.semigroup._

object LoadOrder {

  /**
   * @param deps Map from A to the set of A's that depend on that A
   * @param counts Map from A to the number of unloaded A's that that A depends on
   */
  case class DepGraph[A](deps: Map[A, Set[A]], counts: Map[A, Int]) {

    /**
     * Smash together two dependency graphs by:
     *   Unioning the Sets in deps.
     *   Adding the Ints in counts.
     */
    def ++(dg: DepGraph[A]) = DepGraph[A](deps |+| dg.deps, counts |+| dg.counts)

    /**
     * 'Load' an A:
     *
     *   For everyone who depends on it, decrement their count in the counts map,
     *   because they now depend on one fewer modules that need to be loaded.
     *
     * @param m
     * @return
     */
    def load(m: A) = DepGraph[A](
      deps - m,
      (if(deps contains m) deps(m).map(_ -> -1).toMap |+| counts else counts) - m
    )

    /**
     * Calculate the loadable set, which consists anyone who's count in the counts map is zero.
     * (Because the everything they depended on was loaded).
     * @return
     */
    def loadable: Set[A] = counts.filter{ case (_, i) => i <= 0 }.keySet

    /**
     * Returns a Stream of Sets of A's in the order that they can be loaded.
     * The A's in each Set can be loaded in parallel, because they don't depend on each other.
     */
    def ordered: Stream[Set[A]] = loadable.toList match {
      case Nil => Stream.empty
      case as  => loadable #:: as.foldLeft(this)(_ load _).ordered
    }
  }

  /**
   * Starting from point a, build a DepGraph from the transitive closure of a's dependencies.
   * @param a
   * @param depenencies
   * @tparam A
   * @return
   */
  def getDepGraph[A](a: A, depenencies: A => Set[A]): DepGraph[A] = {
    val depsOfA = depenencies(a).toList
    val dgForA  = DepGraph[A](depsOfA.map(_ -> Set(a)).toMap, Map(a -> depsOfA.size))
    depsOfA.foldLeft(dgForA){ case (dacc, m) => dacc ++ getDepGraph(m, depenencies) }
  }
}
