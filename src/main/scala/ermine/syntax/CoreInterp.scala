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
        def twiddle(v:bound.Var[Int, Core[A]]) = v match {
          case B(0)  => F(a)
          case B(n)  => B(n-1)
          case F(e)  => F(e)
        }
        Lam(n-1, Scope(b.unscope.map(twiddle)))
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
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Core,A] => Core[A]
      lazy val es: Stream[Core[A]] = bs.toStream.map(inst)
      nf(inst(b))
    case cse@Case(c, branches, default) => whnf(c) match {
      case Data(tag, fields) =>
        nf(branches.get(tag).map(instantiate(_)(i => fields(i))).getOrElse(
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
        def twiddle(v:bound.Var[Int, Core[A]]): bound.Var[Int, Core[A]] = v match {
          case B(0)   => F(a)
          case B(n)   => B(n-1)
          case F(e)   => F(e)
        }
        Lam(n-1, Scope(b.unscope.map(twiddle)))
      }

      // hacks for prims
      case p@PrimFun(0, f) => whnf(f.asInstanceOf[Core[A]])
      case p@PrimFun(1, f) => f.asInstanceOf[List[Core[A]] => Core[A]].apply(List(nf(a)))
      case p@PrimFun(n, f) => PartialApp(p, List(whnf(a)))
      case PartialApp(p@PrimFun(n, f), args) =>
        if(args.size + 1 == n) f.asInstanceOf[List[Core[A]] => Core[A]].apply(args :+ nf(a))
        else PartialApp(p, args :+ whnf(a))

      case _          => App(f, a)
    }
    case Let(bs, b)   =>
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Core,A] => Core[A]
      def es: Stream[Core[A]] = bs.toStream.map(inst)
      whnf(inst(b))
    case cs@Case(c, branches, default) => whnf(c) match {
      case Data(tag, fields) =>
        // if there is no branch with the matching tag, the default has to be there
        whnf(branches.get(tag).map(instantiate(_)(i => fields(i))).getOrElse(
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
        def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Core,A] => Core[A]
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

  def indexWhere[A](a: A, as: Seq[A])(implicit e: Equal[A]): Option[Int] = {
    val index = as.indexWhere(_ === a)
    if(index == -1) None else Some(index)
  }

  // combinator for building dictionaries
  def dict(slots: (String, Core[String])*): Core[String] = Dict(Nil, {
    val (x, y) = slots.unzip
    y.map(c => abstrakt(c)(indexWhere(_, x))).toList
  })

  // combinator for building case statements
  def cases(c: Core[String], branches: (Int, (List[String], Core[String]))*): Core[String] = Case(
    c, branches.toMap.mapValues{ case (vars, cr) => abstrakt(cr)(indexWhere(_, vars)) }, None
  )

  //  A smart constructor for Lamb
  def lam[A,F[+_]](vs: A*)(body: Core[A])(implicit m: Monad[F], e: Equal[A]) =
    Lam(vs.size, abstrakt(body)(b => indexWhere(b, vs.toList)))

  def let_[A](es: List[(A, Core[A])], e:Core[A]): Core[A] = es match {
    case Nil => e
    case _ =>
      def abstr(e:Core[A]) = abstractR((a:A) => {
        val i = es.map(_._1).indexOf(a)
        if(i>=0) Some(i) else None
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
  val True: Core[String] = lam("F")(lam("T")(Var("T")))

  val cooked = closed[String, String](let_(List(
    ("False",  "f" !: "t" !: Var("f"))
  , ("True",   "f" !: "t" !: Var("t"))
  , ("if",     "b" !: "t" !: "f" !: Var("b") * Var("f") * Var("t"))
  , ("Zero",   "z" !: "s" !: Var("z"))
  , ("Succ",   "n" !: "z" !: "s" !: Var("s") * Var("n"))
  , ("one",    Var("Succ") * Var("Zero"))
  , ("two",    Var("Succ") * Var("one"))
  , ("three",  Var("Succ") * Var("two"))
  , ("isZero", "n" !: Var("n") * Var("True") * ("m" !: Var("False")))
  , ("const",  "x" !: "y" !: Var("x"))
  , ("Pair",   "a" !: "b" !: "p" !: Var("p") * Var("a") * Var("b"))
  , ("fst",    "ab" !: Var("ab") * ("a" !: "b" !: Var("a")))
  , ("snd",    "ab" !: Var("ab") * ("a" !: "b" !: Var("b")))
  , ("add",    "x" !: "y" !: Var("x") * Var("y") * ("n" !: Var("Succ") * (Var("add") * Var("n") * Var("y"))))
  , ("mul",    "x" !: "y" !: Var("x") * Var("Zero") * ("n" !: Var("add") * Var("y") * (Var("mul") * Var("n") * Var("y"))))
  , ("fac",    "x" !: Var("x") * Var("one") * ("n" !: Var("mul") * Var("x") * (Var("fac") * Var("n"))))
  , ("eqnat",  "x" !: "y" !: Var("x") * (Var("y") * Var("True") * (Var("const") * Var("False"))) * ("x1" !: Var("y") * Var("False") * ("y1" !: Var("eqnat") * Var("x1") * Var("y1"))))
  , ("sumto",  "x" !: Var("x") * Var("Zero") * ("n" !: Var("add") * Var("x") * (Var("sumto") * Var("n"))))
  , ("n5",     Var("add") * Var("two") * Var("three"))
  , ("n6",     Var("add") * Var("three") * Var("three"))
  , ("n17",    Var("add") * Var("n6") * (Var("add") * Var("n6") * Var("n5")))
  , ("n37",    Var("Succ") * (Var("mul") * Var("n6") * Var("n6")))
  , ("n703",   Var("sumto") * Var("n37"))
  , ("n720",   Var("fac") * Var("n6"))
  ), (Var("eqnat") * Var("n720") * (Var("add") * Var("n703") * Var("n17"))))).get

  def main(args: Array[String]){
    println(nf(cooked) === True)
  }
}

object CoreInterpExampleWithData extends CoreInterpExampleHelpers {
  import CoreInterp._

  // Primitive functions
  val Add = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (LitInt(x), LitInt(y)) => LitInt(x + y)
    case e => Err(s"Error in args to +: ${e.toString}")
  })
  val Sub = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (LitInt(x), LitInt(y)) => LitInt(x - y)
    case e => Err(s"Error in args to -: ${e.toString}")
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
    case e            => Err(s"Bad argument to PrintLit: ${e.toString}")
  })

  // Booleans
  val True:  Core[String] = Data(0, Nil)
  val False: Core[String] = Data(1, Nil)

  val EqLit = PrimFun(2, (args:List[Core[String]]) => (nf(args(0)), nf(args(1))) match {
    case (a:Lit,b:Lit) => if(a==b) True else False
    case e => Err(s"Bad argument to PrintLit: ${e.toString}")
  })

  // Pair
  val Pair = "l" !: "r" !: Data(0, List(Var("l"), Var("r")))
  val Fst  = "p" !: Case(Var("p"), Map(0 -> Scope(Var(B(0)))), None)
  val Snd  = "p" !: Case(Var("p"), Map(0 -> Scope(Var(B(1)))), None)

  // List
  val NiL: Core[String]  = Data(0, Nil)
  val Cons  = "head" !: "tail" !: Data(1, List(Var("head"), Var("tail")))
  val Head  = "l" !: Case(Var("l"), Map(0 -> Scope(Err("Can't get the head of Nil")), 1 -> Scope(Var(B(0)))), None)
  val Tail  = "l" !: Case(Var("l"), Map(0 -> Scope(Err("Can't get the tail of Nil")), 1 -> Scope(Var(B(1)))), None)
  val Empty = "l" !: cases(Var("l"), 0 -> (Nil -> True), 1 -> (Nil -> False))

  val Take  = "n" !: "xs" !:
    Var("if") * (eqInt(Var("n"), LitInt(0))) * NiL * (
    Var("if") * (Var("empty") * Var("xs")) * NiL * (
    Var("Cons") * (Var("head") * Var("xs")) * (Var("take") * (Var("-") * Var("n") * LitInt(1)) * Var("xs"))
  ))

  // Ones = 1 : ones
  val Ones = Var("Cons") * LitInt(1) * Var("ones")

  // Dictionaries
  val EqBool = dict(
    "==" -> ("a" !: "b" !: cases(Var("a"),
      0 -> (Nil -> cases(Var("b"), 0 -> (Nil -> True), 1-> (Nil -> False))),
      1 -> (Nil -> cases(Var("b"), 0 -> (Nil -> False), 1 -> (Nil -> True)))
    ))
  )
  def eqb(a:Core[String], b: Core[String]) = AppDict(Slot(0), Var("EqBool")) * a * b
  val ShowBool = dict(
    "show" -> ("b" !: cases(Var("b"), 0 -> (Nil -> LitString("True")), 1 -> (Nil -> LitString("False"))))
  )
  def showBool(c: Core[String]) = (AppDict(Slot(0), Var("ShowBool"))) * c
  val EqInt = dict("==" -> ("a" !: "b" !: (Var("EqLit") * Var("a") * Var("b"))))
  def eqInt(a:Core[String], b: Core[String]) = AppDict(Slot(0), Var("EqInt")) * a * b
  val ShowInt = dict("show" -> ("i" !: Var("printLit") * Var("i")))

  val If = "t" !: "x" !: "y" !: cases(eqb(Var("t"), True), 0 -> (Nil -> Var("x")), 1 -> (Nil -> Var("y")))

  val cooked = closed[String, String](let_(List(
    ("False",    False)
  , ("True",     True)
  , ("one",      LitInt(1))
  , ("Pair",     Pair)
  , ("fst",      Fst)
  , ("snd",      Snd)
  , ("printLit", PrintLit)
  , ("+",        Add)
  , ("-",        Add)
  , ("EqBool",   EqBool)
  , ("ShowBool", ShowBool)
  , ("EqLit",    EqLit)
  , ("EqInt",    EqInt)
  , ("ShowInt",  ShowInt)
  , ("Nil",      NiL)
  , ("Cons",     Cons)
  , ("head",     Head)
  , ("tail",     Tail)
  , ("empty",    Empty)
  , ("ones",     Ones)
  , ("if",       If)
  , ("take",     Take)
  ),
    Var("take") * LitInt(10) * Var("ones")
//    Var("if") * Var("True") * Var("one") * Var("one")
//    showBool(eqb(Var("True"), (Var("snd") * (Var("Pair") * Var("one") * Var("False")))))
  )).get

  def main(args: Array[String]){
    println(nf(cooked))
  }
}
