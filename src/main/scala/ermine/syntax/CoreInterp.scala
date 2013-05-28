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
      case f1        => App(nf(f), nf(a))
    }
    case Let(bs, b)  =>
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Core,A] => Core[A]
      lazy val es: Stream[Core[A]] = bs.toStream.map(inst)
      nf(inst(b))
  }

  def whnf[A](e: Core[A]): Core[A] = e match {
    // TODO: collapse all these => e's into one case at the bottom
    // after finishing the function up.
    case Var(_)       => e
    case h:HardCore   => e
    case Data(_, _)   => e
    case Lam(0, body) => whnf(instantiate(body)(_ => sys.error("f")))
    case Lam(_, _)    => e
    case App(f, a)    => whnf(f) match {
      case Lam(1, b)  => whnf(instantiate1(a, b))
      case Lam(n, b)  => {
        def twiddle(v:bound.Var[Int, Core[A]]): bound.Var[Int, Core[A]] = v match {
          case B(0) => F(a)
          case B(n) => B(n-1)
          case F(e) => F(e)
        }
        Lam(n-1, Scope(b.unscope.map(twiddle)))
      }
      case _          => App(f, a)
    }
    case Let(bs, b)  =>
      def inst = instantiateR((i: Int) => es(i)) _ // Scope[Int,Core,A] => Core[A]
      def es: Stream[Core[A]] = bs.toStream.map(inst)
      whnf(inst(b))
    case Case(c, branches, default) => whnf(c) match {
      case Data(tag, fields) =>
        // if there is no branch with the matching tag, the default has to be there
        whnf(instantiate(branches.getOrElse(tag, default.get))(i => fields(i)))
      case _ => sys.error("not possible...")
    }
    case Dict(_, _)    => e
    case LamDict(_)    => e
    case AppDict(x, y) => whnf(x) match {
      case _ => ???
    }
  }
}


object CoreInterpExample {

  import CoreInterp._

  //  A smart constructor for Lamb
  def lam[A,F[+_]](vs: A*)(body: Core[A])(implicit m: Monad[F], e: Equal[A]) = {
    def findIndex[A](a: A, as: List[(A, Int)])(implicit e: Equal[A]): Option[Int] = as match {
      case Nil => None
      case (x, i) :: xs => if(x === a) Some(i) else findIndex(a, xs)
    }
    Lam(vs.size, abstrakt(body)(b => findIndex(b, vs.toList.zipWithIndex)))
  }

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