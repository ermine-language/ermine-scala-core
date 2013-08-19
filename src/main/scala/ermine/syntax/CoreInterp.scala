package ermine.syntax

import bound._
import bound.Scope._
import scalaz._
import Scalaz._

object CoreInterp {

  def instantiateR[B,F[+_],A](f: B => F[A])(s: Scope[B,F,A])(implicit M: Monad[F]): F[A] =
    instantiate(s)(f)

  def nf[A](e:Core[A]): Core[A] = e match {
    case Var(_)      => e
    case h:HardCore  => h
    case Data(tag, fields) => Data(tag, fields.map(nf))
    case Lam(0, b)   => nf(instantiate(b)(_ => sys.error("f")))
    case Lam(n, b)   => Lam(n, toScope(nf(fromScope(b))))
    case App(f, a)   => whnf(f) match {
      case Lam(1, b) =>  nf(instantiate1(a, b))
      case Lam(n, b) => {
        def twiddle(v:bound.Var[Byte, Core[A]]): bound.Var[Byte, Core[A]] = v match {
          case B(0)  => F(a)
          case B(n)  => B((n-1).toByte)
          case F(e)  => F(e)
        }
        Lam((n-1).toByte, Scope(b.unscope.map(twiddle)))
      }

      // hacks for prims
      case p@PrimFun(0, f) => nf(f.asInstanceOf[Core[A]])
      case p@PrimFun(1, f) => f.asInstanceOf[List[Core[A]] => Core[A]].apply(List(nf(a)))
      case p@PrimFun(n, f) => PartialApp(p, List(whnf(a)))
      case PartialApp(p@PrimFun(n, f), args) =>
        if(args.size + 1 == n) f.asInstanceOf[List[Core[A]] => Core[A]].apply(args :+ nf(a))
        else PartialApp(p, args :+ whnf(a))

      case f1        => App(nf(f), nf(a))
    }
    case Let(bs, b)  =>
      def inst = instantiateR((i: Byte) => es(i.toInt)) _ // Scope[Int,Core,A] => Core[A]
      lazy val es: Stream[Core[A]] = bs.toStream.map(inst)
      nf(inst(b))
    case cse@Case(c, branches, default) => whnf(c) match {
      case Data(tag, fields) =>
        nf(branches.get(tag).map{ case (_, a) => instantiate(a)(i => fields(i)) }.getOrElse(
          instantiate1(Data(tag, fields), default.get)
        ))
      case e@Err(msg) => e
      case x => sys.error(s"not possible... ${whnf(c)}, $x")
    }
    case Dict(_, _)    => sys.error("todo nf Dict")
    case LamDict(_)    => sys.error("todo nf LamDict")
    case AppDict(x, y) => sys.error("todo nf AppDict")

    // hacks for prims
    case PrimFun(_, _) => e
    case PartialApp(_, _) => e
  }

  def whnf[A](e: Core[A]): Core[A] = e match {
    case Var(_)       => e
    case h:HardCore   => e
    case Data(_, _)   => e
    case Lam(0, body) => whnf(instantiate(body)(_ => sys.error("f")))
    case Lam(_, _)    => e
    case App(f, a)    => whnf(f) match {
      case Err(msg)   => e
      case Lam(1, b)  => whnf(instantiate1(a, b))
      case Lam(n, b)  => {
        def twiddle(v:bound.Var[Byte, Core[A]]): bound.Var[Byte, Core[A]] = v match {
          case B(0)  => F(a)
          case B(n)  => B((n-1).toByte)
          case F(e)  => F(e)
        }
        Lam((n-1).toByte, Scope(b.unscope.map(twiddle)))
      }

      // hacks for prims
      case p@PrimFun(0, f) => whnf(f.asInstanceOf[Core[A]])
      case p@PrimFun(1, f) => f.asInstanceOf[List[Core[A]] => Core[A]].apply(List(whnf(a)))
      case p@PrimFun(n, f) => PartialApp(p, List(whnf(a)))
      case PartialApp(p@PrimFun(n, f), args) =>
        if(args.size + 1 == n) f.asInstanceOf[List[Core[A]] => Core[A]].apply(args :+ whnf(a))
        else PartialApp(p, args :+ whnf(a))

      case _          => App(f, a)
    }
    case Let(bs, b)   =>
      def inst = instantiateR((i: Byte) => es(i.toInt)) _ // Scope[Int,Core,A] => Core[A]
      def es: Stream[Core[A]] = bs.toStream.map(inst)
      whnf(inst(b))
    case cs@Case(c, branches, default) => whnf(c) match {
      case Data(tag, fields) =>
        // if there is no branch with the matching tag, the default has to be there
        whnf(branches.get(tag).map{ case (_, a) => instantiate(a)(i => fields(i)) }.getOrElse(
          instantiate1(Data(tag, fields), default.get)
        ))
      case e@Err(msg) => e
      case x => sys.error(s"not possible: $x, $cs")
    }
    case Dict(_, _)    => e
    case LamDict(_)    => e
    case AppDict(x, y) => (whnf(x), whnf(y)) match {
      case (Err(msg), _) => Err(msg)
      case (Super(i), Dict(sups, _))  => sups(i)
      case (Slot(i),  Dict(_, slots)) =>
        def inst = instantiateR((i: Byte) => es(i.toInt)) _ // Scope[Int,Core,A] => Core[A]
        def es: Stream[Core[A]] = slots.toStream.map(inst)
        whnf(es(i))
      case (LamDict(body), dict@Dict(_, _)) => whnf(instantiate1(dict, body))
      case _ => sys.error("not possible.")
    }

    // hacks for prims
    case PrimFun(_, _) => e
    case PartialApp(_, _) => e
  }
}

trait CoreInterpExampleHelpers {

  implicit class MkVar(val sc: StringContext) {
    def v(args: Any*): Core[String] = Var(sc.parts.mkString)
  }

  def indexWhere[A](a: A, as: Seq[A])(implicit e: Equal[A]): Option[Byte] = {
    val index = as.indexWhere(_ === a).toByte
    if(index == -1) None else Some(index)
  }

  // combinator for building dictionaries
  def dict(slots: (String, Core[String])*): Dict[String] = Dict(Nil, {
    val (x, y) = slots.unzip
    y.map(c => abstrakt(c)(indexWhere(_, x))).toList
  })

  // combinator for building case statements
  def cases(c: Core[String], branches: (Int, (List[String], Core[String]))*): Case[String] = Case(
    c, branches.toMap.mapKeys(_.toByte).mapValues{
      case (vars, cr) => ((0:Byte), abstrakt(cr)(indexWhere(_, vars).map((x:Byte) => (x + 1).toByte)))
    }, None
  )

  def caseIgnoreArity[V](c: Core[V], branches: Map[Byte, Scope[Byte, Core, V]], default: Option[Scope[Unit, Core, V]]): Case[V] =
    Case(c, branches.mapValues(s => (0, s)), default)

  //  A smart constructor for Lamb
  def lam[A,F[+_]](vs: A*)(body: Core[A])(implicit m: Monad[F], e: Equal[A]): Lam[A] =
    Lam(vs.size.toByte, abstrakt(body)(b => indexWhere(b, vs.toList)))

  def let_[A](es: List[(A, Core[A])], e:Core[A]): Core[A] = es match {
    case Nil => e
    case _ =>
      def abstr(e:Core[A]) = abstractR((a:A) => {
        val i = es.map(_._1).indexOf(a)
        if(i>=0) Some(i.toByte) else None
      })(e)
      Let(es.map(t => abstr(t._2)), abstr(e))
  }

  def abstractR[B,F[+_],A](f : A => Option[B])(w : F[A])(implicit M: scalaz.Monad[F]) = abstrakt(w)(f)

  implicit class PimpedCore(e: Core[String]) {
    def !:(s:String) = lam[String, Core](s)(e)
  }

  def closed[A, B](fa:Core[A]): Option[Core[B]] = {
    implicit val x = Core.coreTraversable
    fa.traverse(Function.const(None))
  }
}


object CoreInterpExample extends CoreInterpExampleHelpers {

  import CoreInterp._

  //  true :: Core String
  //  true = lam "F" $ lam "T" $ V "T"
  val True: Core[String] = lam("F")(lam("T")(v"T"))

  val cooked = closed[String, String](let_(List(
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
    println(nf(cooked) === True)
  }
}

object CoreInterpExampleWithData extends CoreInterpExampleHelpers {
  import CoreInterp._

  // Primitive functions
  val Add = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (LitInt(x), LitInt(y)) => LitInt(x + y)
    case e => Err(s"Error in args to +: $e")
  })
  val Sub = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (LitInt(x), LitInt(y)) => LitInt(x - y)
    case e => Err(s"Error in args to -: $e")
  })
  val StringAppend = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (LitString(x), LitString(y)) => LitString(x + y)
    case e => Err(s"Error in args to stringAppend: $e")
  })

  val PrintLit = PrimFun(1, (args:List[Core[String]]) => nf(args(0)) match {
    case LitInt(x)    => LitString(x.toString)
    case LitInt64(l)  => LitString(l.toString)
    case LitByte(b)   => LitString(b.toString)
    case LitShort(s)  => LitString(s.toString)
    case LitString(s) => LitString(s.toString)
    case LitChar(c)   => LitString(c.toString)
    case LitFloat(f)  => LitString(f.toString)
    case LitDouble(d) => LitString(d.toString)
    case e            => Err(s"Bad argument to PrintLit: $e")
  })

  // Booleans
  val True:  Core[String] = Data(0, Nil)
  val False: Core[String] = Data(1, Nil)

  val EqLit = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (a:Lit,b:Lit) => if(a==b) True else False
    case e => Err(s"Bad argument to EqLit: $e")
  })

  // Pair
  val Pair = "l" !: "r" !: Data(0, List(v"l", v"r"))
  val Fst  = "p" !: caseIgnoreArity(v"p", Map(0.toByte -> Scope(Var(B(0.toByte)))), None)
  val Snd  = "p" !: caseIgnoreArity(v"p", Map(0.toByte -> Scope(Var(B(1.toByte)))), None)

  // List
  val NiL: Core[String]  = Data(0, Nil)
  val Cons  = "head" !: "tail" !: Data(1, List(v"head", v"tail"))
  val Head  = "l" !: caseIgnoreArity(v"l", Map(0.toByte -> Scope(Err("Can't get the head of Nil")), 1.toByte -> Scope(Var(B(0.toByte)))), None)
  val Tail  = "l" !: caseIgnoreArity(v"l", Map(0.toByte -> Scope(Err("Can't get the tail of Nil")), 1.toByte -> Scope(Var(B(1.toByte)))), None)
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

  // Dictionaries
  val EqBool = dict(
    "==" -> ("a" !: "b" !: cases(v"a",
      0 -> (Nil -> cases(v"b", 0 -> (Nil -> True), 1-> (Nil -> False))),
      1 -> (Nil -> cases(v"b", 0 -> (Nil -> False), 1 -> (Nil -> True)))
    ))
  )
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), v"EqBool") * a * b
  val ShowBool = dict(
    "show" -> ("b" !: cases(v"b", 0 -> (Nil -> LitString("True")), 1 -> (Nil -> LitString("False"))))
  )
  def showBool(c: Core[String]) = (AppDict(Slot(0), v"ShowBool")) * c
  val EqInt = dict("==" -> ("a" !: "b" !: (v"EqLit" * v"a" * v"b")))
  def eqInt(a:Core[String], b: Core[String]) = AppDict(Slot(0), v"EqInt") * a * b
  val ShowInt = dict("show" -> ("i" !: v"printLit" * v"i"))

  val JoinStringList = "l" !: cases(v"l",
    0 -> (Nil -> LitString("")),
    1 -> (List("x", "xs") -> v"stringAppend" * v"x" * (v"joinStringList" * v"xs"))
  )

  def ShowList(sup: Dict[String]): Dict[String] = dict(
    "show" -> ("l" !: cases(v"intersperse" * LitString(",") * (v"map" * (AppDict(Slot(0), sup)) * v"l"),
      0 -> (Nil -> LitString("[]")),
      1 -> (List("h", "t") -> v"stringAppend" * (v"stringAppend" * LitString("[") * (v"joinStringList" * (v"Cons" * v"h" * v"t"))) *  LitString("]"))
  ))).copy(supers=List(sup))

  val ShowBoolList = ShowList(ShowBool)
  val ShowIntList  = ShowList(ShowInt)
  def showBoolList(c: Core[String]) = (AppDict(Slot(0), v"ShowBoolList")) * c
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

  val cooked = closed[String, String](let_(List(
    ("False",    False)
  , ("True",     True)
  , ("one",      LitInt(1))
  , ("Pair",     Pair)
  , ("fst",      Fst)
  , ("snd",      Snd)
  , ("printLit", PrintLit)
  , ("+",        Add)
  , ("-",        Sub)
  , ("stringAppend", StringAppend)
  , ("joinStringList", JoinStringList)
  , ("EqBool",   EqBool)
  , ("ShowBool", ShowBool)
  , ("EqLit",    EqLit)
  , ("EqInt",    EqInt)
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
  , ("replicate" , Replicate)
  , ("ones",     Ones)
  , ("if",       If)
  , ("take",     Take)
  , ("append",   ListAppend)
  , ("concat",   ListConcat)
  , ("ListMonad", ListMonad)
  , ("oneThroughThree", v"Cons" * LitInt(1) * (v"Cons" * LitInt(2) * (v"Cons" * LitInt(3) * NiL)))
  , ("oneThroughFive",  v"Cons" * LitInt(1) * (v"Cons" * LitInt(2) * (v"Cons" * LitInt(3) * (v"Cons" * LitInt(4) * (v"Cons" * LitInt(5) * NiL)))))
  ,("ttff", v"Cons" * v"True" * (v"Cons" * v"True" * (v"Cons" * v"False" * (v"Cons" * v"False" * NiL))))
  ), showBoolList(v"ttff")
  //showIntList((AppDict(Slot(1), v"ListMonad")) * v"oneThroughFive" * ("x" !: (v"replicate" * v"x" * v"x")))
//  showIntList(v"intersperse" * LitInt(7) * (v"map" * (v"+" * LitInt(2)) * (v"take" * LitInt(10) * v"ones")))
//  v"if" * v"True" * v"one" * v"one"
//  showBool(eqb(v"True", (v"snd" * (v"Pair" * v"one" * v"False"))))
  )).get

  def main(args: Array[String]){
    println(nf(cooked) match {
      case LitString(s) => s
      case x => x.toString
    })
  }
}
