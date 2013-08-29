package ermine
package eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import Core._

object EvalExamples extends ErmineProperties("CoreSerializationTests") with CoreCombinators {

  val prelude = module("Prelude",
    "seq" -> (("a","b") !: gcases(v"a", "_", Some(v"b")))
  )()

  val boolModule = module("Bool",
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

  val stringModule = module("String",
    ("stringAppend", PrimOp("stringAppend"))
  , ("stringValueOfInt", ForeignMethod(static=true, "java.lang.String", "valueOf", List("int")))
  , ("emptyString", ForeignConstructor("java.lang.String", Nil))
  , ("stringCopy" , ForeignConstructor("java.lang.String", List("java.lang.String")))
  , ("toLowerCase", ForeignMethod(static=false, "java.lang.String", "toLowerCase", Nil))
  , ("replaceChar", ForeignMethod(static=false, "java.lang.String", "replace", List("char", "char")))
  )()

  val intModule = module("Int",
    "-" -> PrimOp("minusInt"),
    "+" -> PrimOp("plusInt")
  )(
    "eqInt" -> dict("==" -> (("a", "b") !: (PrimOp("eqInt") * v"a" * v"b"))),
    "showInt" -> dict("show" -> ("i" !: g"String.stringValueOfInt" * v"i"))
  )

  val tupleModule = module("Tuple",
    "Pair" -> (("l", "r") !: CoreData(0, List(v"l", v"r"))),
    "fst"  -> ("p" !: cases(v"p", (0, (List("a", "b"), v"a")))),
    "snd"  -> ("p" !: cases(v"p", (0, (List("a", "b"), v"b"))))
  )()

  val mathModule = module("Math",
    "pi" -> ForeignValue(static=true, "java.lang.Math", "PI")
  )()

  val listModule = module("List",
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
      1 -> (List("x", "xs") -> g"String.stringAppend" * v"x" * (v"joinStringList" * v"xs"))
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

  def ShowList: Core[String] = lamDict("d")(dict(
    "show" -> ("l" !: cases(g"List.intersperse" * LitString(",") * (g"List.map" * AppDict(Slot(0), v"d") * v"l"),
      0 -> (Nil -> LitString("[]")),
      1 -> (List("h", "t") -> g"String.stringAppend" * (g"String.stringAppend" * LitString("[") * (g"List.joinStringList" * (g"List.Cons" * v"h" * v"t"))) *  LitString("]"))
  ))))

  // helper functions
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), i"eqBool") * a * b
  def showBool(c: Core[String]) = AppDict(Slot(0), i"showBool") * c
  def eqInt(a:Core[String], b: Core[String]) = AppDict(Slot(0), i"eqInt") * a * b
  def showBoolList(c: Core[String]) = AppDict(Slot(0), i"showBoolList") * c
  def showIntList(c: Core[String])  = AppDict(Slot(0), i"showIntList")  * c
  def mkList(input:Core[String]*) = input.foldRight[Core[String]](GlobalRef(g"List.Nil")){ (c, acc) => g"List.Cons" * c * acc }

  def evl(c:Core[String]) = {
    Eval.sessionEnv = SessionEnv.load(prelude, stringModule, boolModule, intModule, tupleModule, mathModule, listModule)
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

  evalTest("showBool True", showBool(g"Bool.True"), Prim("True"))
  evalTest("pair", showBool(eqb(g"Bool.True", g"Tuple.snd" * (g"Tuple.Pair" * 1 * g"Bool.False"))), Prim("False"))

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
}
