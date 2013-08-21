package ermine.syntax

import bound._
import bound.Scope._
import scalaz._
import Scalaz._

object Native {
  def getClassFor(name: String): Class[_] = name match {
    case "boolean" => classOf[Boolean]
    case "byte"    => classOf[Byte]
    case "char"    => classOf[Char]
    case "short"   => classOf[Short]
    case "int"     => classOf[Int]
    case "long"    => classOf[Long]
    case "float"   => classOf[Float]
    case "double"  => classOf[Double]
    case "void"    => java.lang.Void.TYPE
    case _         => Class.forName(name)
  }
}

import Native._

/**
data Core a
  = Var a
  | HardCore !HardCore
  | Data !Word8 [Core a]
  | App !(Core a) !(Core a)
  | Lam !Word8 !(Scope Word8 Core a)
  | Let [Scope Word8 Core a] !(Scope Word8 Core a)
  | Case !(Core a) (Map Word8 (Word8, Scope Word8 Core a)) (Maybe (Scope () Core a))
  | Dict { supers :: [Core a], slots :: [Scope Word8 Core a] }
  | LamDict !(Scope () Core a)
  | AppDict !(Core a) !(Core a)
  deriving (Eq,Show,Read,Functor,Foldable,Traversable)
 */
sealed trait HardCore extends Core[Nothing]
  case class Super(b: Byte)        extends HardCore
  case class Slot (b: Byte)        extends HardCore
  case class Err  (msg: String)    extends HardCore
  sealed trait Lit extends HardCore { def extract: Any }
  case class LitInt   (extract: Int)     extends Lit
  case class LitInt64 (extract: Long)    extends Lit
  case class LitByte  (extract: Byte)    extends Lit
  case class LitShort (extract: Short)   extends Lit
  case class LitString(extract: String)  extends Lit
  case class LitChar  (extract: Char)    extends Lit
  case class LitFloat (extract: Float)   extends Lit
  case class LitDouble(extract: Double)  extends Lit

  case class PrimOp(name: String) extends HardCore

  case class ForeignMethod(static: Boolean, className: String, methodName: String, argumentTypes: List[String]) extends HardCore {
    // TODO: maybe catch an error here if class is not found
    lazy val method = Class.forName(className).getMethod(methodName, argumentTypes.map(getClassFor):_*)
    lazy val arity = method.getParameterTypes.length.toByte
  }

  case class ForeignConstructor(className: String, argumentTypes: List[String]) extends HardCore {
    // TODO: maybe catch an error here if class is not found
    lazy val con: java.lang.reflect.Constructor[_] = Class.forName(className).getConstructor(argumentTypes.map(getClassFor):_*)
    lazy val arity = con.getParameterTypes.length.toByte
  }

  case class ForeignValue(static: Boolean, className: String, fieldName: String) extends HardCore {
    // TODO: maybe catch an error here if class is not found
    lazy val field: java.lang.reflect.Field = Class.forName(className).getField(fieldName)
  }


object HardCore {
  implicit val hardcoreEqual: Equal[HardCore] = Equal.equalA[HardCore]
}

sealed trait Core[+V]{
  def *[B >: V](e:Core[B]) = App(this, e)
}
  object Var {
    def apply[V](a: => V): Core[V] = new Var(a)
    def unapply[V](e: Core[V]): Option[V] = e match {
      case v:Var[V] => Some(v.get)
      case _ => None
    }
  }
  class Var[+V](a: => V) extends Core[V]{
    lazy val get = a
    override def toString = s"Var($a)"
  }

  object Data {
    def apply[V](tag: => Byte, fields: => List[Core[V]]): Core[V] = new Data(tag, fields)
    def unapply[V](e: Core[V]): Option[(Byte, List[Core[V]])] = e match {
      case v:Data[V] => Some(v.get)
      case _ => None
    }
  }
  class Data[+V](tag: => Byte, fields: => List[Core[V]]) extends Core[V]{
    lazy val get = (tag, fields)
    override def toString = s"Data($tag, $fields)"
  }

  object App {
    def apply[V](f: => Core[V], x: => Core[V]): Core[V] = new App(f, x)
    def unapply[V](e: Core[V]): Option[(Core[V], Core[V])] = e match {
      case v:App[V] => Some(v.get)
      case _ => None
    }
  }
  class App[+V](f: => Core[V], x: => Core[V]) extends Core[V]{
    lazy val get = (f, x)
    override def toString = s"App($f, $x)"
  }

  case class Lam[+V](arity: Byte, body: Scope[Byte, Core, V])                          extends Core[V]
  case class Let[+V](bindings: List[Scope[Byte, Core, V]], body: Scope[Byte, Core, V]) extends Core[V]
  case class Case[+V](c: Core[V], branches: Map[Byte, (Byte, Scope[Byte, Core, V])], default: Option[Scope[Unit, Core, V]]) extends Core[V]
  case class Dict[+V](supers: List[Core[V]], slots: List[Scope[Byte, Core, V]])        extends Core[V]
  case class LamDict[+V](body: Scope[Unit, Core, V])                                   extends Core[V]
  case class AppDict[+V](f: Core[V], d: Core[V])                                       extends Core[V]

object Core {

  implicit def coreEqual[V: Equal]: Equal[Core[V]] = new Equal[Core[V]] {
    def equal(a: Core[V], b: Core[V]): Boolean = (a, b) match {
      case (Var(v1), Var(v2)) => v1 === v2
      case (h1:HardCore, h2:HardCore) => h1 === h2
      case (Data(tag1, fields1), Data(tag2, fields2)) => tag1 === tag2 && fields1 === fields2
      case (App(f1, x1), App(f2, x2)) => f1 === f2 && x1 === x2
      case (Lam(arity1, body1), Lam(arity2, body2)) => arity1 === arity2 && body1 === body2
      case (Let(bindings1, body1), Let(bindings2, body2)) => bindings1 === bindings2 && body1 === body2
      case (Case(c1, branches1, def1), Case(c2, branches2, def2)) => c1 === c2 && branches1 === branches2 && def1 === def2
      case (Dict(supers1, slots1), Dict(supers2, slots2)) => supers1 === supers2 && slots1 === slots2
      case (LamDict(body1), LamDict(body2)) => body1 === body2
      case (AppDict(f1, d1), AppDict(f2, d2)) => f1 === f2 && d1 === d2
      case _ => false
    }
  }

  implicit val coreEqual1: Equal1[Core] = new Equal1[Core] {
    def equal[V: Equal](c1: Core[V], c2: Core[V]) = c1 === c2
  }

  implicit val coreMonad: Monad[Core] = new Monad[Core]{
    def point[A](a: => A) = Var(a)
    def bind[A,B](c: Core[A])(f: A => Core[B]): Core[B] = c match {
      case Var(a)         => f(a)
      case h: HardCore    => h
      case Data(n, xs)    => Data(n, xs.map(c => bind(c)(f)))
      case App(x, y)      => App(bind(x)(f), bind(y)(f))
      case Lam(n, e)      => Lam(n, e >>>= f)
      case Let(bs, e)     => Let(bs.map(s => s >>>= f), e >>>= f)
      case Case(e, bs, d) => Case(bind(e)(f), bs.mapValues{ case (b, a) => (b, a >>>= f) }, d.map(_ >>>= f))
      case Dict(xs, ys)   => Dict(xs.map(c => bind(c)(f)), ys.map(s => s >>>= f))
      case LamDict(e)     => LamDict(e >>>= f)
      case AppDict(x, y)  => AppDict(bind(x)(f), bind(y)(f))
    }
  }

  private def print(name: Cord, parts: Cord*): Cord = Cord(name, "(", Cord.mkCord(", ", parts: _*), ")")
  implicit def coreShow[V: Show]: Show[Core[V]] = Show.show[Core[V]](_ match {
    case Var(a)         => print("Var", a.show)
    case Super(b)       => print("Super", b.show)
    case Slot(b)        => print("Slot", b.show)
    case Err(msg)       => print("Err", msg)
    case LitInt(i)      => print("LitInt", i.show)
    case LitInt64(l)    => print("LitInt64", l.show)
    case LitByte(b)     => print("LitByte", b.show)
    case LitShort(s)    => print("LitShort", s.show)
    case LitString(s)   => print("LitString", s)
    case LitChar(c)     => print("LitChar", c.show)
    case LitFloat(f)    => print("LitFloat", f.show)
    case LitDouble(d)   => print("LitDouble", d.show)
    case Data(n, xs)    => print("Data", n.show, xs.show)
    case App(x, y)      => print("App", x.show, y.show)
    case Lam(n, e)      => print("Lam", n.show, e.show)
    case Let(bs, e)     => print("Let", bs.show, e.show)
    case Case(e, bs, d) => print("Case", bs.show, d.show)
    case Dict(xs, ys)   => print("Dict", xs.show, ys.show)
    case LamDict(e)     => print("LamDict", e.show)
    case AppDict(x, y)  => print("AppDict", x.show, y.show)
  })

  val coreTraversable: Traverse[Core] = new Traverse[Core]{
    def traverseImpl[F[+_], A, B](exp : Core[A])(f : A => F[B])(implicit A: Applicative[F]) : F[Core[B]] = {
      def traverseScope[V](s: Scope[V, Core, A]) = s.traverse(f)(A, coreTraversable)
      exp match {
        case Var(a)         => f(a).map(Var(_))
        case h: HardCore    => A.point(h)
        case Data(n, xs)    => xs.traverse(traverse(_)(f)).map(Data(n, _))
        case App(x, y)      => A.apply2(traverse(x)(f), traverse(y)(f))(App(_, _))
        case Lam(a, e)      => e.traverse(f)(A, coreTraversable).map(Lam(a, _))
        case Let(bs, b)     => A.apply2(bs.traverse(traverseScope), b.traverse(f)(A, coreTraversable))(Let(_, _))
        case Case(e, bs, d) => A.apply3(
          traverse(e)(f),
          bs.toList.traverse{ case (i, (b, s)) => traverseScope(s).map(a => (i, (b, a))) }.map(_.toMap),
          d.traverse(traverseScope)
        )(Case(_, _, _))
        case Dict(xs, ys)   => A.apply2(xs.traverse(traverse(_)(f)), ys.traverse(traverseScope))(Dict(_, _))
        case LamDict(e)     => traverseScope(e).map(LamDict(_))
        case AppDict(x, y)  => A.apply2(traverse(x)(f), traverse(y)(f))(AppDict(_, _))
      }
    }
  }
}
