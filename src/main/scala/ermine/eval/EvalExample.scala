package ermine.eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import bound._
import Core._

object EvalExampleSimple extends CoreInterpExampleHelpers {

  //  true :: Core String
  //  true = lam "F" $ lam "T" $ V "T"
  val True: Core[String] = lam("F")(lam("T")(v"T"))

  val cooked = closed[String, Address](let_(List(
    ("False",  "f" !: "t" !: v"f")
  , ("True",   "f" !: "t" !: v"t")
  , ("if",     "b" !: "t" !: "f" !: v"b" * v"f" * v"t")
  , ("Zero",   "z" !: "s" !: v"z")
  , ("Succ",   "n" !: "z" !: "s" !: v"s" * v"n")
  , ("one",    v"Succ" * v"Zero")
  , ("two",    v"Succ" * v"one")
  , ("three",  v"Succ" * v"two")
  , ("isZero", "n" !: v"n" * v"True" * ("m" !: v"False"))
  , ("const",  "x" !: "y" !: v"x")
  , ("Pair",   "a" !: "b" !: "p" !: v"p" * v"a" * v"b")
  , ("fst",    "ab" !: v"ab" * ("a" !: "b" !: v"a"))
  , ("snd",    "ab" !: v"ab" * ("a" !: "b" !: v"b"))
  , ("add",    "x" !: "y" !: v"x" * v"y" * ("n" !: v"Succ" * (v"add" * v"n" * v"y")))
  , ("mul",    "x" !: "y" !: v"x" * v"Zero" * ("n" !: v"add" * v"y" * (v"mul" * v"n" * v"y")))
  , ("fac",    "x" !: v"x" * v"one" * ("n" !: v"mul" * v"x" * (v"fac" * v"n")))
  , ("eqnat",  "x" !: "y" !: v"x" * (v"y" * v"True" * (v"const" * v"False")) * ("x1" !: v"y" * v"False" * ("y1" !: v"eqnat" * v"x1" * v"y1")))
  , ("sumto",  "x" !: v"x" * v"Zero" * ("n" !: v"add" * v"x" * (v"sumto" * v"n")))
  , ("n5",     v"add" * v"two" * v"three")
  , ("n6",     v"add" * v"three" * v"three")
  , ("n17",    v"add" * v"n6" * (v"add" * v"n6" * v"n5"))
  , ("n37",    v"Succ" * (v"mul" * v"n6" * v"n6"))
  , ("n703",   v"sumto" * v"n37")
  , ("n720",   v"fac" * v"n6")
  ), v"eqnat" * v"n720" * (v"add" * v"n703" * v"n17"))).get

  def main(args: Array[String]){
    println(Eval.whnf(Eval.eval(Map(), cooked)))
  }
}

object EvalExampleWithData extends CoreInterpExampleHelpers {

  // Booleans
  val True:  Core[String] = CoreData(0, Nil)
  val False: Core[String] = CoreData(1, Nil)

  def eqb = "a" !: "b" !: cases(v"a",
    0 -> (Nil -> cases(v"b", 0 -> (Nil -> True), 1-> (Nil -> False))),
    1 -> (Nil -> cases(v"b", 0 -> (Nil -> False), 1 -> (Nil -> True)))
  )

  // Pair
  val Pair = "l" !: "r" !: CoreData(0, List(v"l", v"r"))
  val Fst  = "p" !: caseIgnoreArity(v"p", Map(0.toByte -> Scope(Var(B(0.toByte)))), None)
  val Snd  = "p" !: caseIgnoreArity(v"p", Map(0.toByte -> Scope(Var(B(1.toByte)))), None)

  // List
  val NiL: Core[String]  = CoreData(0, Nil)
  val Cons  = "head" !: "tail" !: CoreData(1, List(v"head", v"tail"))
  val Head  = "l" !: caseIgnoreArity(v"l", Map(0.toByte -> Scope(Err("Can't get the head of Nil")), 1.toByte -> Scope(Var(B(0.toByte)))), None)
  val Tail  = "l" !: caseIgnoreArity(v"l", Map(0.toByte -> Scope(Err("Can't get the tail of Nil")), 1.toByte -> Scope(Var(B(1.toByte)))), None)
  val Empty = "l" !: cases(v"l", 0 -> (Nil -> True), 1 -> (Nil -> False))
  def singleton(a: Core[String]) = v"Cons" * a * NiL

  val ListMap = "f" !: "l" !: cases(v"l",
    0 -> (Nil -> NiL),
    1 -> (List("h", "t") -> v"Cons" * (v"f" * v"h") * (v"map" * v"f" * v"t"))
  )

  val ListAppend = "xs" !: "ys" !: cases(v"xs",
    0 -> (Nil -> v"ys"),
    1 -> (List("h", "t") -> v"Cons" * v"h" * (v"append" * v"t" * v"ys"))
  )

  val ListConcat = "xs" !: cases(v"xs",
    0 -> (Nil -> NiL),
    1 -> (List("h", "t") -> v"append" * v"h" * (v"concat" * v"t"))
  )

  val Intersperse = "sep" !: "l" !: cases(v"l",
    0 -> (Nil -> NiL),
    1 -> (List("x", "xs") -> v"Cons" * v"x" * (v"prependToAll" * v"sep" * v"xs")
  ))

  val PrependToAll = "sep" !: "l" !: cases(v"l",
    0 -> (Nil -> NiL),
    1 -> (List("x", "xs") -> v"Cons" * v"sep" * (v"Cons" * v"x" * (v"prependToAll" * v"sep" * v"xs"))
  ))

  // ones = 1 : ones
  val Ones = v"Cons" * LitInt(1) * v"ones"

  val JoinStringList = "l" !: cases(v"l",
    0 -> (Nil -> LitString("")),
    1 -> (List("x", "xs") -> v"stringAppend" * v"x" * (v"joinStringList" * v"xs"))
  )

  val If = "t" !: "x" !: "y" !: cases(eqb * v"t" * True, 0 -> (Nil -> v"x"), 1 -> (Nil -> v"y"))

  val cooked = closed[String, Address](let_(List(
    ("False",    False)
  , ("True",     True)
  , ("one",      LitInt(1))
  , ("Cons",     Cons)
  , ("ones",     Ones)
  , ("if",       If)
  ),  v"ones"
    //v"if" * v"True" * v"one" * v"one"
  )).get

  def main(args: Array[String]){
    println(Eval.whnf(Eval.eval(Map(), cooked)))
  }
}
