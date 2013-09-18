package ermine
package eval

import scalaz._
import Scalaz._
import ermine.syntax._
import TestLib._

object EvalExamples extends ErmineProperties("EvalExamples") with CoreCombinators {

  evalTest("showBool True", showBool(g"Bool.True"), Prim("True"))
  evalTest("pair fst'", g"Tuple.fst" * (g"Tuple.Pair" * 1 * 1), Prim(1))
  evalTest("pair snd", showBool(eqb(g"Bool.True", g"Tuple.snd" * (g"Tuple.Pair" * 1 * g"Bool.False"))), Prim("False"))
  evalTest("True", showBool(g"Bool.True"), Prim("True"))
  evalTest(
    "show bool list",
    showBoolList(mkList(g"Bool.True", g"Bool.True", g"Bool.False", g"Bool.False")),
    Prim("[True,True,False,False]")
  )
  evalTest("stringValueOfInt", g"String.stringValueOfInt" * 1, Prim("1"))
  evalTest("show int list", showIntList(mkList(50, 1000, 1, 1)), Prim("[50,1000,1,1]"))
  evalTest("take", showIntList(g"List.take" * 5 * g"List.ones"), Prim("[1,1,1,1,1]"))
  evalTest(
    "list monad",
    showIntList(AppDict(Slot(1), i"listMonad") * (g"List.take" * 5 * g"List.nats") * ("x" !: (g"List.replicate" * v"x" * v"x"))),
    Prim("[1,2,2,3,3,3,4,4,4,4]")
  )
  evalTest(
    "intersperse",
    showIntList(g"List.intersperse" * 7 * (g"List.map" * (g"Int.+" * 2) * (g"List.take" * 10 * g"List.ones"))),
    Prim("[3,7,3,7,3,7,3,7,3,7,3,7,3,7,3,7,3,7,3]")
  )
  evalTest("if", g"Bool.if" * g"Bool.True" * 1 * 1, Prim(1))
  evalTest("constructor with no args",    g"String.emptyString", Prim(""))
  evalTest("constructor with args",       g"String.stringCopy"  * LitString("cartman"), Prim("cartman"))
  evalTest("instance function,  no args", g"String.toLowerCase" * LitString("CartMan"), Prim("cartman"))
  evalTest("instance function with args", g"String.replaceChar" * LitString("F*CK") * LitChar('*') * LitChar('!'), Prim("F!CK"))
  evalTest("static value", g"Math.pi", Prim(math.Pi))
  evalTest(
    "instance value",
    ForeignValue(static=false, "ermine.eval.Dummy", "x") * ForeignConstructor("ermine.eval.Dummy", Nil),
    Prim(5)
  )
  evalTestBottom("err", Err("death!"), "death!")
  evalTestBottom("seq with err", g"Prelude.seq" * Err("seq death!") * 5, "seq death!")

  def evl(c:Core[String]) = {
    val allModules = List(prelude, stringModule, boolModule, intModule, tupleModule, mathModule, listModule)
    val modulesMap = allModules.map(m => m.name -> m).toMap
    val ordered = LoadOrder.getDepGraph(allModules:_*)(m => m.dependencies.map(modulesMap).toSet).ordered
    Eval.sessionEnv = SessionEnv.load(ordered.force.flatten:_*)
    Eval.whnf(Eval.eval(Eval.sessionEnv.env, closed(c).get))
  }
  def evalTest(name: String, c:Core[String], expectedResult:Prim) = test(name){
    //println(evl(c))
    evl(c) == expectedResult
  }
  def evalTestBottom(name: String, c:Core[String], expectedErrorMessage: String) = test(name){
    val res = evl(c)
    res match {
      case b:Bottom => b.render === s"<exception: $expectedErrorMessage>"
      case x => false
    }
  }
}
