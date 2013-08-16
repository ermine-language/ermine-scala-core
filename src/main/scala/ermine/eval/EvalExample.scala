package ermine.eval

import ermine.syntax.{Core, CoreInterpExampleHelpers}
import scalaz._
import Scalaz._
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
  ), (v"eqnat" * v"n720" * (v"add" * v"n703" * v"n17")))).get

  def main(args: Array[String]){
    println(Eval.whnf(Eval.eval(Map(), cooked)))
  }
}

