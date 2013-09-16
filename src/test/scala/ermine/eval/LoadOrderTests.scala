package ermine.eval

import ermine.ErmineProperties
import TestLib._
import SessionEnv._
import LoadOrder._
import scalaz._
import Scalaz._

object LoadOrderTests extends ErmineProperties("Load Order") {

  val diamond: Map[String, Set[String]] = Map(
    "z" -> Set("x", "y"),
    "x" -> Set("a"),
    "y" -> Set("a"),
    "a" -> Set())
  testDeps("diamond:z")("z", diamond)(List(List("a"), List("x", "y"), List("z")))
  testDeps("diamond:x")("x", diamond)(List(List("a"), List("x")))
  testDeps("diamond:a")("a", diamond)(List(List("a")))

  testDeps("diamond")("z", diamond)(List(List("a"), List("x", "y"), List("z")))
  val twoRoots: Map[String, Set[String]] = Map(
    "z" -> Set("x", "y"),
    "x" -> Set(),
    "y" -> Set("a"),
    "a" -> Set("b"),
    "b" -> Set())
  testDeps("twoRoots:z")("z", twoRoots)(List(List("x", "b"), List("a"), List("y"), List("z")))
  testDeps("twoRoots:y")("y", twoRoots)(List(List("b"), List("a"), List("y")))

  val modules  = List(prelude, stringModule, boolModule, intModule, tupleModule, mathModule, listModule)
  val modDeps = modules.map(m => m.name.toString -> m.dependencies.map(_.toString).toSet).toMap
  testDeps("modules")(listModule.name.toString, modDeps)(
    List(List("ermine.Bool", "ermine.Int", "ermine.String"), List("ermine.List"))
  )

  def testDeps[A : Equal](name: String)(a: A, deps: Map[A, Set[A]])(expected: List[List[A]]) = test(name){
    val res = getDepGraph(a, deps).ordered.toList.map(_.toList)
    println(res)
    res === expected
  }
}
