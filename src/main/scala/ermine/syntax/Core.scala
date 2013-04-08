package ermine.syntax

import bound._
import bound.Scope._
import scalaz._
import Scalaz._

/**
data Core a
  = Var a
  | HardCore !HardCore
  | Data !Int [Core a]
  | App !(Core a) !(Core a)
  | Lam !Int !(Scope Int Core a)
  | Let [Scope Int Core a] !(Scope Int Core a)
  | Case !(Core a) [Branch a] -- TODO: IntMap?
  | Dict { supers :: [Core a], slots :: [Scope Int Core a] }
  | LamDict !(Scope () Core a)
  | AppDict !(Core a) !(Core a)
  deriving (Eq,Show,Read,Functor,Foldable,Traversable)
 */
sealed trait HardCore extends Core[Nothing]
  case class Super(i: Int)        extends HardCore
  case class Slot(i: Int)         extends HardCore
  case class LitInt(i: Int)       extends HardCore
  case class LitInt64(l: Long)    extends HardCore
  case class LitByte(b: Byte)     extends HardCore
  case class LitShort(s: Short)   extends HardCore
  case class LitString(s: String) extends HardCore
  case class LitChar(c: Char)     extends HardCore
  case class LitFloat(f: Float)   extends HardCore
  case class LitDouble(d: Double) extends HardCore

object HardCore {
  implicit def hardcoreEqual: Equal[HardCore] = new Equal[HardCore] {
    def equal(a: HardCore, b: HardCore) = a == b
  }
}

sealed trait Branch[+V] { def body: Scope[Int, Core, V] }
  case class Labeled[+V](tag: Int, body: Scope[Int, Core, V]) extends Branch[V]
  case class Default[+V](          body: Scope[Int, Core, V]) extends Branch[V]

object Branch {
  implicit def branchEqual[V](implicit VE: Equal[V]): Equal[Branch[V]] = new Equal[Branch[V]] {
    def equal(a: Branch[V], b: Branch[V]): Boolean = (a, b) match {
      case (Labeled(tag1, body1), Labeled(tag2, body2)) => tag1 === tag2 && body1 === body2
      case (Default(      body1), Default(      body2)) =>                  body1 === body2
      case _ => false
    }
  }
  def boundBy[A,B](b: Branch[A])(f: A => Core[B]): Branch[B] = b match {
    case Labeled(tag, body) => Labeled(tag, body >>>= f)
    case Default(     body) => Default(     body >>>= f)
  }
}

sealed trait Core[+V]
  case class Var[+V](v: V)                                                           extends Core[V]
  case class Data[+V](tag: Int, fields: List[Core[V]])                               extends Core[V]
  case class App[+V](f: Core[V], x: Core[V])                                         extends Core[V]
  case class Lam[+V](arity: Int, body: Scope[Int, Core, V])                          extends Core[V]
  case class Let[+V](bindings: List[Scope[Int, Core, V]], body: Scope[Int, Core, V]) extends Core[V]
  case class Case[+V](c: Core[V], branches: List[Branch[V]])                         extends Core[V]
  case class Dict[+V](supers: List[Core[V]], slots: List[Scope[Int, Core, V]])       extends Core[V]
  case class LamDict[+V](body: Scope[Unit, Core, V])                                 extends Core[V]
  case class AppDict[+V](f: Core[V], d: Core[V])                                     extends Core[V]

object Core {

  implicit def coreEqual[V](implicit VE: Equal[V]): Equal[Core[V]] = new Equal[Core[V]] {
    def equal(a: Core[V], b: Core[V]): Boolean = (a, b) match {
      case (Var(v1), Var(v2)) => v1 === v2
      case (Data(tag1, fields1), Data(tag2, fields2)) => tag1 === tag2 && fields1 === fields2
      case (App(f1, x1), App(f2, x2)) => f1 === f2 && x1 === x2
      case (Lam(arity1, body1), Lam(arity2, body2)) => arity1 === arity2 && body1 === body2
      case (Let(bindings1, body1), Let(bindings2, body2)) => bindings1 === bindings2 && body1 === body2
      case (Case(c1, branches1), Case(c2, branches2)) => c1 === c2 && branches1 === branches2
      case (Dict(supers1, slots1), Dict(supers2, slots2)) => supers1 === supers2 && slots1 === slots2
      case (LamDict(body1), LamDict(body2)) => body1 === body2
      case (AppDict(f1, d1), AppDict(f2, d2)) => f1 === f2 && d1 === d2
      case _ => false
    }
  }

  implicit def coreEqual1: Equal1[Core] = new Equal1[Core] {
    def equal[V](c1: Core[V], c2: Core[V])(implicit ve: Equal[V]) = c1 === c2
  }

  implicit def coreMonad: Monad[Core] = new Monad[Core]{
    def point[A](a: => A) = Var(a)
    def bind[A,B](c: Core[A])(f: A => Core[B]): Core[B] = c match {
      case Var(a)        => f(a)
      case h: HardCore   => h
      case Data(n, xs)   => Data(n, xs.map(c => bind(c)(f)))
      case App(x, y)     => App(bind(x)(f), bind(y)(f))
      case Lam(n, e)     => Lam(n, e >>>= f)
      case Let(bs, e)    => Let(bs.map(s => s >>>= f), e >>>= f)
      case Case(e, as)   => Case(bind(e)(f), as.map(a => Branch.boundBy(a)(f)))
      case Dict(xs, ys)  => Dict(xs.map(c => bind(c)(f)), ys.map(s => s >>>= f))
      case LamDict(e)    => LamDict(e >>>= f)
      case AppDict(x, y) => AppDict(bind(x)(f), bind(y)(f))
    }
  }
}
