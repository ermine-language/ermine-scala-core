package ermine.eval

import scalaz._
import std.map._, std.set._, std.anyVal._, syntax.semigroup._


object LoadOrder {
  type DepSet[A] = Map[A, Set[A]]
  type Counts[A] = Map[A, Int]

  case class DepGraph[A](ds: DepSet[A], counts: Counts[A], loadable: Set[A]) {
    def ++(other: DepGraph[A]) = DepGraph[A](ds |+| other.ds, counts |+| other.counts, loadable ++ other.loadable)

    def load(m: A): DepGraph[A] = {
      val newCounts: Counts[A] = if(ds contains m) ds(m).map(_ -> -1).toMap |+| counts else counts
      val newLoadableSet: Set[A] = (newCounts.filter{ case (_, i) => i <= 0 }.keySet ++ loadable) - m
      DepGraph[A](ds - m, newCounts - m, newLoadableSet)
    }

    def ordered: Stream[Set[A]] = loadable.toList match {
      case Nil => Stream.empty
      case as  => loadable #:: as.foldLeft(this)(_ load _).ordered
    }
  }

  def getDepGraph[A](a: A, depenencies: A => Set[A]): DepGraph[A] = {
    val depsOfA = depenencies(a).toList
    val d = DepGraph[A](
      depsOfA.map(_ -> Set(a)).toMap,
      Map(a -> depsOfA.size),
      if(depsOfA.isEmpty) Set(a) else Set()
    )
    depsOfA.foldLeft(d){ case (dacc, m) => dacc ++ getDepGraph(m, depenencies) }
  }
}
