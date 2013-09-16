package ermine.syntax

import bound._
import bound.Scope._
import scalaz._
import Scalaz._
import syntax.order, syntax.semigroup
import f0.{Read, Sources}
import java.security.MessageDigest

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

sealed trait Assoc
  case object L extends Assoc
  case object R extends Assoc
  case object N extends Assoc

object Assoc {
  implicit val assocOrder: Order[Assoc] = new Order[Assoc] {
    override def equalIsNatural = true
    def order(l: Assoc, r: Assoc) = (l, r) match {
      case (L, L) => mzero[Ordering]
      case (R, R) => mzero[Ordering]
      case (N, N) => mzero[Ordering]
      case (L, _) => Ordering.LT
      case (_, L) => Ordering.GT
      case (R, _) => Ordering.LT
      case (N, _) => Ordering.GT
    }
  }
}

trait Fixity
  case class Infix(assoc:Assoc, level:Int) extends Fixity
  case class Prefix(level:Int) extends Fixity
  case class Postfix(level:Int) extends Fixity
  case object Idfix extends Fixity

object Fixity {
  implicit val globalOrder: Order[Fixity] = new Order[Fixity] {
    override def equalIsNatural = true
    def order(l: Fixity, r: Fixity) = (l, r) match {
      case (Infix(a1, l1), Infix(a2, l2)) => a1 ?|? a2 |+|  l1 ?|? l2
      case (Prefix(l1),    Prefix(l2))    => l1 ?|? l2
      case (Postfix(l1),   Postfix(l2))   => l1 ?|? l2
      case (Idfix,         Idfix)         => mzero[Ordering]
      case (Infix(_, _), _)               => Ordering.LT
      case (_, Infix(_, _))               => Ordering.GT
      case (Idfix, _)                     => Ordering.GT
      case (_, Idfix)                     => Ordering.LT
      case (Prefix(_), _)                 => Ordering.LT
      case (Postfix(_), _)                => Ordering.GT
    }
  }
}

object Digest{
  def apply(bytes: Array[Byte], orig: Option[String] = None): Digest = {
    val s = Sources.fromArray(bytes)
    new Digest(Read.longF(s), Read.longF(s)){
      override val original = orig.orElse(Some(new String(bytes)))
    }
  }

  def apply(strings: String*) : Digest = {
    val md = MessageDigest.getInstance("MD5")
    strings.foreach(s => md.update(s.getBytes))
    Digest(md.digest, Some(strings.mkString(".")))
  }
  implicit val globalOrder: Order[Digest] = Order.order((l, r) => l.part1 ?|? r.part1 |+|  l.part2 ?|? r.part2)
}
case class Digest(part1: Long, part2: Long){
  val original: Option[String] = None
}

object ModuleName {
  def apply(pkg: String, name: String): ModuleName = {
    new ModuleName(Digest(pkg, name), pkg, name)
  }
  implicit val globalOrder: Order[ModuleName] = Order.order((l, r) => l.pkg ?|? r.pkg |+|  l.name ?|? r.name)
}
case class ModuleName(digest: Digest, pkg: String, name: String){
  override def toString = s"$pkg.$name"
}

object Global {
  def apply(module: ModuleName, name: String, fixity: Fixity = Idfix): Global = {
    new Global(Digest(module.pkg, module.name, name), fixity, module, name)
  }
  implicit val globalOrder: Order[Global] = new Order[Global] {
    override def equalIsNatural = Equal[ModuleName].equalIsNatural && Equal[Fixity].equalIsNatural
    override def equal(l: Global, r: Global) = if (equalIsNatural) l == r else super.equal(l, r)
    def order(l: Global, r: Global) = (l, r) match {
      case (Global(_, f1, m1, n1), Global(_, f2, m2, n2)) => m1 ?|? m2 |+|  n1 ?|? n2 |+| f1 ?|? f2
    }
  }
}
case class Global(digest: Digest, fixity: Fixity, module: ModuleName, name: String)

case class Module[+A](
  name:         ModuleName,
  dependencies: List[ModuleName],
  definitions:  Vector[Core[A]],
  termExports:  Map[Global, Either[Global, A]],
  instances:    Map[Digest, A]) {
  def map[B](f: A => B): Module[B] =
    copy(
      definitions = definitions.map(_.map(f)),
      termExports = termExports.mapValues(_.right.map(f)),
      instances   = instances.mapValues(f)
    )
}

object Module {
  implicit def ModuleEqual[V: Equal]: Equal[Module[V]] = new Equal[Module[V]]{
    def equal(a: Module[V], b: Module[V]): Boolean = (a, b) match {
      case (Module(aname, adeps, adefs, aterms, ainsts), Module(bname, bdeps, bdefs, bterms, binsts)) =>
        aname === bname && adeps === bdeps && adefs === bdefs && aterms === bterms && ainsts === binsts
    }
  }

  implicit val moduleEqual1: Equal1[Module] = new Equal1[Module] {
    def equal[V: Equal](m1: Module[V], m2: Module[V]) = m1 === m2
  }
}

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

  case class PrimOp(name: String)   extends HardCore
  case class GlobalRef(g: Global)   extends HardCore
  case class InstanceRef(d: Digest) extends HardCore

  sealed trait Foreign extends HardCore
  case class ForeignMethod(static: Boolean, className: String, methodName: String, argumentTypes: List[String]) extends Foreign {
    // TODO: maybe catch an error here if class is not found
    lazy val method = Class.forName(className).getMethod(methodName, argumentTypes.map(getClassFor):_*)
    lazy val arity = method.getParameterTypes.length.toByte
  }

  case class ForeignConstructor(className: String, argumentTypes: List[String]) extends Foreign {
    // TODO: maybe catch an error here if class is not found
    lazy val con: java.lang.reflect.Constructor[_] = Class.forName(className).getConstructor(argumentTypes.map(getClassFor):_*)
    lazy val arity = con.getParameterTypes.length.toByte
  }

  case class ForeignValue(static: Boolean, className: String, fieldName: String) extends Foreign {
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

  implicit val coreMonad: Traverse[Core] with Monad[Core] = new Traverse[Core] with Monad[Core]{
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

    // TODO: this + is unsound; see https://github.com/scalaz/scalaz/pull/328
    def traverseImpl[F[+_], A, B](exp : Core[A])(f : A => F[B])(implicit A: Applicative[F]) : F[Core[B]] = {
      def traverseScope[V](s: Scope[V, Core, A]) = s.traverse(f)
      exp match {
        case Var(a)         => f(a).map(Var(_))
        case h: HardCore    => A.point(h)
        case Data(n, xs)    => xs.traverse(traverse(_)(f)).map(Data(n, _))
        case App(x, y)      => A.apply2(traverse(x)(f), traverse(y)(f))(App(_, _))
        case Lam(a, e)      => e.traverse(f).map(Lam(a, _))
        case Let(bs, b)     => A.apply2(bs.traverse(traverseScope), b.traverse(f))(Let(_, _))
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
}
