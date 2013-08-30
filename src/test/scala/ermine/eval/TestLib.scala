package ermine
package eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import Core._

object TestLib extends CoreCombinators {
  val prelude = module("Prelude")(
    "seq" -> (("a","b") !: gcases(v"a", "_", Some(v"b")))
  )()

  val boolModule = module("Bool")(
    "False" -> CoreData(0, Nil),
    "True"  -> CoreData(1, Nil),
    "if"    -> (("t", "x", "y") !: cases(v"t", 0 -> (Nil -> v"y"), 1 -> (Nil -> v"x")))
  )(
    "showBool" -> dict(
      "show" -> ("b" !: cases(v"b", 0 -> (Nil -> LitString("False")), 1 -> (Nil -> LitString("True"))))
    ),
    "eqBool" -> dict(
      "==" -> ("a" !: "b" !: cases(v"a",
        0 -> (Nil -> cases(v"b", 0 -> (Nil -> g"Bool.True"),  1 -> (Nil -> g"Bool.False"))),
        1 -> (Nil -> cases(v"b", 0 -> (Nil -> g"Bool.False"), 1 -> (Nil -> g"Bool.True")))
      ))
    )
  )

  val stringModule = module("String")(
    ("append", PrimOp("stringAppend"))
  , ("stringValueOfInt", ForeignMethod(static=true, "java.lang.String", "valueOf", List("int")))
  , ("emptyString", ForeignConstructor("java.lang.String", Nil))
  , ("stringCopy" , ForeignConstructor("java.lang.String", List("java.lang.String")))
  , ("toLowerCase", ForeignMethod(static=false, "java.lang.String", "toLowerCase", Nil))
  , ("replaceChar", ForeignMethod(static=false, "java.lang.String", "replace", List("char", "char")))
  )()

  val intModule = module("Int")(
    "-" -> PrimOp("minusInt"),
    "+" -> PrimOp("plusInt")
  )(
    "eqInt" -> dict("==" -> (("a", "b") !: (PrimOp("eqInt") * v"a" * v"b"))),
    "showInt" -> dict("show" -> ("i" !: g"String.stringValueOfInt" * v"i"))
  )

  val tupleModule = module("Tuple")(
    "Pair" -> (("l", "r") !: CoreData(0, List(v"l", v"r"))),
    "fst"  -> ("p" !: cases(v"p", (0, (List("a", "b"), v"a")))),
    "snd"  -> ("p" !: cases(v"p", (0, (List("a", "b"), v"b"))))
  )()

  val mathModule = module("Math")(
    "pi" -> ForeignValue(static=true, "java.lang.Math", "PI")
  )()

  val listModule = module("List")(
    "Nil" -> CoreData(0, Nil),
    "Cons" -> (("head", "tail") !: CoreData(1, List(v"head", v"tail"))),
    "take"  -> (("n", "xs") !:
      g"Bool.if" * eqInt(v"n", 0) * v"Nil" * cases(v"xs",
        0 -> (Nil -> v"Nil"),
        1 -> (List("h", "t") -> v"Cons" * v"h" * (v"take" * (g"Int.-" * v"n" * 1) * v"t"))
      )),
    "head"  -> ("l" !: cases(v"l", (0, (List(), Err("Can't get the head of Nil"))), (1, (List("a", "rest"), v"a")))),
    "tail"  -> ("l" !: cases(v"l", (0, (List(), Err("Can't get the head of Nil"))), (1, (List("a", "rest"), v"rest")))),
    "empty" -> ("l" !: cases(v"l", 0 -> (Nil -> g"Bool.False"), 1 -> (Nil -> g"Bool.False"))),
    "replicate" -> (("n", "a") !:
      g"Bool.if" * eqInt(v"n", 0) * v"Nil" * (v"Cons" * v"a" * (v"replicate" * (g"Int.-" * v"n" * 1) * v"a"))),
    "map" -> (("f", "l") !: cases(v"l",
      0 -> (Nil -> v"Nil"),
      1 -> (List("h", "t") -> v"Cons" * (v"f" * v"h") * (v"map" * v"f" * v"t"))
    )),
    "append" -> (("xs", "ys") !: cases(v"xs",
      0 -> (Nil -> v"ys"),
      1 -> (List("h", "t") -> v"Cons" * v"h" * (v"append" * v"t" * v"ys"))
    )),
    "concat" -> ("xs" !: cases(v"xs",
      0 -> (Nil -> v"Nil"),
      1 -> (List("h", "t") -> v"append" * v"h" * (v"concat" * v"t"))
    )),
    "intersperse" -> (("sep", "l") !: cases(v"l",
      0 -> (Nil -> v"Nil"),
      1 -> (List("x", "xs") -> v"Cons" * v"x" * (v"prependToAll" * v"sep" * v"xs"))
    )),
    "prependToAll" -> (("sep", "l") !: cases(v"l",
      0 -> (Nil -> v"Nil"),
      1 -> (List("x", "xs") -> v"Cons" * v"sep" * (v"Cons" * v"x" * (v"prependToAll" * v"sep" * v"xs"))
    ))),
    "joinStringList" -> ("l" !: cases(v"l",
      0 -> (Nil -> LitString("")),
      1 -> (List("x", "xs") -> g"String.append" * v"x" * (v"joinStringList" * v"xs"))
    )),
    // ones = 1 : ones
    "ones" -> v"Cons" * 1 * v"ones",
    "nats" -> v"Cons" * 0 * (v"map" * (g"Int.+" * 1) * v"nats")
  )(
    "listFunctor" -> dict("fmap" -> v"map"),
    "listAp" -> dict(
      "pure" -> ("a" !: mkList(v"a")),
      "<*>"  -> (("f", "a") !: (v"concat" * (v"map" * ("x" !: (v"map" * ("y" !: v"x" * v"y")) * v"f") * v"a")))
    ),
    "listMonad" -> dict(
      "unit" -> ("a" !: mkList(v"a")),
      ">>="  -> (("xs", "f") !: (v"concat" * (v"map" * v"f" * v"xs")))
    ),
    "showBoolList" -> AppDict(ShowList,i"showBool"),
    "showIntList"  -> AppDict(ShowList,i"showInt")
  )

  // helper functions
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), i"eqBool") * a * b
  def showBool(c: Core[String]) = AppDict(Slot(0), i"showBool") * c
  def eqInt(a:Core[String], b: Core[String]) = AppDict(Slot(0), i"eqInt") * a * b
  def showBoolList(c: Core[String]) = AppDict(Slot(0), i"showBoolList") * c
  def showIntList(c: Core[String])  = AppDict(Slot(0), i"showIntList")  * c
  def mkList(input:Core[String]*) = input.foldRight[Core[String]](GlobalRef(g"List.Nil")){ (c, acc) => g"List.Cons" * c * acc }
  def ShowList: Core[String] = lamDict("d")(dict(
    "show" -> ("l" !: cases(g"List.intersperse" * LitString(",") * (g"List.map" * AppDict(Slot(0), v"d") * v"l"),
      0 -> (Nil -> LitString("[]")),
      1 -> (List("h", "t") -> g"String.append" * (g"String.append" * LitString("[") * (g"List.joinStringList" * (g"List.Cons" * v"h" * v"t"))) *  LitString("]"))
  ))))
}
