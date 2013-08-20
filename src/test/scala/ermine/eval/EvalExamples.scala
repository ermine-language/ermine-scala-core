package ermine
package eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import Core._

object EvalExamples extends ErmineProperties("CoreSerializationTests") with CoreCombinators {

  // Booleans
  val False: Core[String] = CoreData(0, Nil)
  val True:  Core[String] = CoreData(1, Nil)

  // Pair
  val Pair = "l" !: "r" !: CoreData(0, List(v"l", v"r"))
  val Fst = "p" !: cases(v"p", (0, (List("a", "b"), v"a")))
  val Snd = "p" !: cases(v"p", (0, (List("a", "b"), v"b")))

  // List
  val NiL: Core[String]  = CoreData(0, Nil)
  val Cons  = "head" !: "tail" !: CoreData(1, List(v"head", v"tail"))

  val Head = "l" !: cases(v"l", (0, (List(), Err("Can't get the head of Nil"))), (1, (List("a", "rest"), v"a")))
  val Tail = "l" !: cases(v"l", (0, (List(), Err("Can't get the head of Nil"))), (1, (List("a", "rest"), v"rest")))
  val Empty = "l" !: cases(v"l", 0 -> (Nil -> True), 1 -> (Nil -> False))
  def singleton(a: Core[String]) = v"Cons" * a * NiL

  val Take  = "n" !: "xs" !:
    v"if" * eqInt(v"n", LitInt(0)) * NiL * cases(v"xs",
      0 -> (Nil -> NiL),
      1 -> (List("h", "t") -> v"Cons" * v"h" * (v"take" * (v"-" * v"n" * LitInt(1)) * v"t"))
    )

  val Replicate  = "n" !: "a" !:
    v"if" * eqInt(v"n", LitInt(0)) * NiL * (v"Cons" * v"a" * (v"replicate" * (v"-" * v"n" * LitInt(1)) * v"a"))

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
  val Nats = v"Cons" * LitInt(0) * (v"map" * (v"+" * v"one") * v"nats")

  // Dictionaries
  val EqBool = dict(
    "==" -> ("a" !: "b" !: cases(v"a",
      0 -> (Nil -> cases(v"b", 0 -> (Nil -> True),  1 -> (Nil -> False))),
      1 -> (Nil -> cases(v"b", 0 -> (Nil -> False), 1 -> (Nil -> True)))
    ))
  )
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), v"EqBool") * a * b
  val ShowBool = dict(
    "show" -> ("b" !: cases(v"b", 0 -> (Nil -> LitString("False")), 1 -> (Nil -> LitString("True"))))
  )
  def showBool(c: Core[String]) = AppDict(Slot(0), v"ShowBool") * c
  val ShowInt = dict("show" -> ("i" !: v"stringValueOfInt" * v"i"))
  val JoinStringList = "l" !: cases(v"l",
    0 -> (Nil -> LitString("")),
    1 -> (List("x", "xs") -> v"stringAppend" * v"x" * (v"joinStringList" * v"xs"))
  )

  val EqInt = dict("==" -> ("a" !: "b" !: (PrimOp("eqInt") * v"a" * v"b")))
  def eqInt(a:Core[String], b: Core[String]) = AppDict(Slot(0), v"EqInt") * a * b

  def ShowList(sup: Dict[String]): Dict[String] = dict(
    "show" -> ("l" !: cases(v"intersperse" * LitString(",") * (v"map" * AppDict(Slot(0), sup) * v"l"),
      0 -> (Nil -> LitString("[]")),
      1 -> (List("h", "t") -> v"stringAppend" * (v"stringAppend" * LitString("[") * (v"joinStringList" * (v"Cons" * v"h" * v"t"))) *  LitString("]"))
  ))).copy(supers=List(sup))

  val ShowBoolList = ShowList(ShowBool)
  val ShowIntList  = ShowList(ShowInt)
  def showBoolList(c: Core[String]) = AppDict(Slot(0), v"ShowBoolList") * c
  def showIntList(c: Core[String]) = AppDict(Slot(0), v"ShowIntList") * c

  val If = "t" !: "x" !: "y" !: cases(eqb(v"t", True), 0 -> (Nil -> v"y"), 1 -> (Nil -> v"x"))

  val ListFunctor = dict("fmap" -> ListMap)
  val ListAp      = dict(
    "pure" -> ("a" !: singleton(v"a")),
    "<*>"  -> ("f" !: "a" !: (v"concat" * (v"map" * ("x" !: (v"map" * ("y" !: v"x" * v"y")) * v"f") * v"a")))
  )
  val ListMonad   = dict(
    "unit" -> ("a" !: singleton(v"a")),
    ">>="  -> ("xs" !: "f" !: (v"concat" * (v"map" * v"f" * v"xs")))
  )

  def cooked(c:Core[String]) = closed(let_(List(
    ("False",    False)
  , ("True",     True)
  , ("one",      LitInt(1))
  , ("Pair",     Pair)
  , ("fst",      Fst)
  , ("snd",      Snd)
  , ("joinStringList", JoinStringList)
  , ("EqBool",   EqBool)
  , ("ShowBool", ShowBool)
  , ("EqInt",    EqInt)
  , ("stringAppend", PrimOp("stringAppend"))
  , ("-",        PrimOp("minusInt"))
  , ("+",        PrimOp("plusInt"))
  , ("ShowInt",  ShowInt)
  , ("Nil",      NiL)
  , ("Cons",     Cons)
  , ("ShowBoolList", ShowBoolList)
  , ("ShowIntList" , ShowIntList)
  , ("head",     Head)
  , ("tail",     Tail)
  , ("empty",    Empty)
  , ("map",      ListMap)
  , ("prependToAll", PrependToAll)
  , ("intersperse" , Intersperse)
  , ("replicate",    Replicate)
  , ("ones",      Ones)
  , ("nats",      Nats)
  , ("if",        If)
  , ("take",      Take)
  , ("append",    ListAppend)
  , ("concat",    ListConcat)
  , ("ListMonad", ListMonad)
  , ("stringValueOfInt", ForiegnMethod(static=true, "java.lang.String", "valueOf", List("int")))
  , ("now",         ForiegnConstructor("java.util.Date", Nil))
  , ("emptyString", ForiegnConstructor("java.lang.String", Nil))
  , ("stringCopy" , ForiegnConstructor("java.lang.String", List("java.lang.String")))
  , ("toLowerCase", ForiegnMethod(static=false, "java.lang.String", "toLowerCase", Nil))
  , ("replaceChar", ForiegnMethod(static=false, "java.lang.String", "replace", List("char", "char")))
  , ("pi", ForiegnValue(static=true, "java.lang.Math", "PI"))
  ), c
  )).get

  def evalTest(name: String, c:Core[String], expectedResult:Prim) = test(name)(
    Eval.whnf(Eval.eval(Map(), cooked(c))) == expectedResult
  )

  evalTest(
    "show bool list",
    showBoolList(v"Cons" * v"True" * (v"Cons" * v"True" * (v"Cons" * v"False" * (v"Cons" * v"False" * NiL)))),
    Prim("[True,True,False,False]")
  )
  evalTest(
    "stringValueOfInt",
    v"stringValueOfInt" * v"one",
    Prim("1")
  )
  evalTest(
    "show int list",
    showIntList(v"Cons" * LitInt(50) * (v"Cons" * LitInt(1000) * (v"Cons" * LitInt(1) * (v"Cons" * v"one" * NiL)))),
    Prim("[50,1000,1,1]")
  )
  evalTest(
    "take",
    showIntList(v"take" * LitInt(5) * v"ones"),
    Prim("[1,1,1,1,1]")
  )
  evalTest(
    "list monad",
    showIntList(AppDict(Slot(1), v"ListMonad") * (v"take" * LitInt(5) * v"nats") * ("x" !: (v"replicate" * v"x" * v"x"))),
    Prim("[1,2,2,3,3,3,4,4,4,4]")
  )
  evalTest(
    "intersperse",
    showIntList(v"intersperse" * LitInt(7) * (v"map" * (v"+" * LitInt(2)) * (v"take" * LitInt(10) * v"ones"))),
    Prim("[3,7,3,7,3,7,3,7,3,7,3,7,3,7,3,7,3,7,3]")
  )
  evalTest(
    "if",
    v"if" * v"True" * v"one" * v"one",
    Prim(1)
  )
  evalTest(
    "pair",
    showBool(eqb(v"True", v"snd" * (v"Pair" * v"one" * v"False"))),
    Prim("False")
  )
  evalTest("constructor with no args",    v"emptyString", Prim(""))
  evalTest("constructor with args",       v"stringCopy" * LitString("cartman"), Prim("cartman"))
  evalTest("instance function,  no args", v"toLowerCase" * LitString("CartMan"), Prim("cartman"))
  evalTest("instance function with args", v"replaceChar" * LitString("F*CK") * LitChar('*') * LitChar('!'), Prim("F!CK"))
  evalTest("static value", v"pi", Prim(math.Pi))
  evalTest(
    "instance value",
    ForiegnValue(static=false, "ermine.eval.Dummy", "x") * ForiegnConstructor("ermine.eval.Dummy", Nil),
    Prim(5)
  )
}
