package ermine.eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import bound._
import Core._

object EvalExampleWithDataAndTypeClasses extends CoreInterpExampleHelpers {
  import CoreInterp._

  // Booleans
  val True:  Core[String] = CoreData(0, Nil)
  val False: Core[String] = CoreData(1, Nil)

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

//  val Take  = "n" !: "xs" !:
//    v"if" * eqInt(v"n", LitInt(0)) * NiL * cases(v"xs",
//      0 -> (Nil -> NiL),
//      1 -> (List("h", "t") -> v"Cons" * v"h" * (v"take" * (v"-" * v"n" * LitInt(1)) * v"t"))
//    )
//
//  val Replicate  = "n" !: "a" !:
//    v"if" * eqInt(v"n", LitInt(0)) * NiL * (v"Cons" * v"a" * (v"replicate" * (v"-" * v"n" * LitInt(1)) * v"a"))

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

  // Dictionaries
  val EqBool = dict(
    "==" -> ("a" !: "b" !: cases(v"a",
      0 -> (Nil -> cases(v"b", 0 -> (Nil -> True),  1 -> (Nil -> False))),
      1 -> (Nil -> cases(v"b", 0 -> (Nil -> False), 1 -> (Nil -> True)))
    ))
  )
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), v"EqBool") * a * b
  val ShowBool = dict(
    "show" -> ("b" !: cases(v"b", 0 -> (Nil -> LitString("True")), 1 -> (Nil -> LitString("False"))))
  )
  def showBool(c: Core[String]) = AppDict(Slot(0), v"ShowBool") * c
  val ShowInt = dict("show" -> ("i" !: v"stringValueOfInt" * v"i"))
  val JoinStringList = "l" !: cases(v"l",
    0 -> (Nil -> LitString("")),
    1 -> (List("x", "xs") -> v"stringAppend" * v"x" * (v"joinStringList" * v"xs"))
  )

  def ShowList(sup: Dict[String]): Dict[String] = dict(
    "show" -> ("l" !: cases(v"intersperse" * LitString(",") * (v"map" * AppDict(Slot(0), sup) * v"l"),
      0 -> (Nil -> LitString("[]")),
      1 -> (List("h", "t") -> v"stringAppend" * (v"stringAppend" * LitString("[") * (v"joinStringList" * (v"Cons" * v"h" * v"t"))) *  LitString("]"))
  ))).copy(supers=List(sup))

  val ShowBoolList = ShowList(ShowBool)
  val ShowIntList  = ShowList(ShowInt)
  def showBoolList(c: Core[String]) = AppDict(Slot(0), v"ShowBoolList") * c
  def showIntList(c: Core[String]) = (AppDict(Slot(0), v"ShowIntList")) * c

  val If = "t" !: "x" !: "y" !: cases(eqb(v"t", True), 0 -> (Nil -> v"x"), 1 -> (Nil -> v"y"))

  val ListFunctor = dict("fmap" -> ListMap)
  val ListAp      = dict(
    "pure" -> ("a" !: singleton(v"a")),
    "<*>"  -> ("f" !: "a" !: (v"concat" * (v"map" * ("x" !: (v"map" * ("y" !: v"x" * v"y")) * v"f") * v"a")))
  )
  val ListMonad   = dict(
    "unit" -> ("a" !: singleton(v"a")),
    ">>="  -> ("xs" !: "f" !: (v"concat" * (v"map" * v"f" * v"xs")))
  )

  val stringAppendAddr = new Address
  val stringAppend : Runtime = Func(2, { case List(pre, post) => (Eval.whnf(pre), Eval.whnf(post)) match {
    case (Prim(x : String), Prim(y : String)) => Prim(x + y)
    case _ => sys.error("no.")
  }})

  def resolveTopLevel(s: String): Address = s match {
    case "stringAppend" => stringAppendAddr
  }
  def cooked(c:Core[String]) = let_(List(
    ("False",    False)
  , ("True",     True)
  , ("one",      LitInt(1))
  , ("Pair",     Pair)
  , ("fst",      Fst)
  , ("snd",      Snd)
  , ("joinStringList", JoinStringList)
  , ("EqBool",   EqBool)
  , ("ShowBool", ShowBool)
  //, ("EqInt",    EqInt)
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
  //, ("replicate" , Replicate)
  , ("ones",     Ones)
  , ("if",       If)
  //, ("take",     Take)
  , ("append",   ListAppend)
  , ("concat",   ListConcat)
  , ("ListMonad", ListMonad)
  , ("stringValueOfInt", ForiegnFunc("java.lang.String", "valueOf", List("int")))
  ), c
  ).map(resolveTopLevel)

  val initialEnv : Map[Address,Runtime] = Map(
    stringAppendAddr -> stringAppend
  )
  val StringAppend = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
      case (LitString(x), LitString(y)) => LitString(x + y)
      case e => Err(s"Error in args to stringAppend: $e")
    })
  def main(args: Array[String]){
    println(Eval.whnf(Eval.eval(initialEnv, cooked(showBoolList(v"Cons" * v"True" * (v"Cons" * v"True" * (v"Cons" * v"False" * (v"Cons" * v"False" * NiL))))))))
    println(Eval.whnf(Eval.eval(initialEnv, cooked(v"stringValueOfInt" * v"one"))))
    println(Eval.whnf(Eval.eval(initialEnv, cooked(
      showIntList(v"Cons" * LitInt(50) * (v"Cons" * LitInt(1000) * (v"Cons" * LitInt(1) * (v"Cons" * v"one" * NiL)))))))
    )
  }
}
