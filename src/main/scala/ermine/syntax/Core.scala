package ermine.syntax

import bound._

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
trait Branch[+V] { def body: Scope[Int, Core, V] }
  case class Labeled[+V](tag: Int, body: Scope[Int, Core, V]) extends Branch[V]
  case class Default[+V](body: Scope[Int, Core, V]) extends Branch[V]

trait Core[+V]
  case class Var[+V](v: V) extends Core[V]
  case class Data[+V](tag: Int, fields: List[Core[V]]) extends Core[V]
  case class App[+V](f: Core[V], x: Core[V]) extends Core[V]
  case class Lam[+V](arity: Int, body: Scope[Int, Core, V]) extends Core[V]
  case class Let[+V](bindings: List[Scope[Int, Core, V]], body: Scope[Int, Core, V]) extends Core[V]
  case class Case[+V](c: Core[V], branches: List[Branch[V]]) extends Core[V]
  case class Dict[+V](supers: List[Core[V]], slots: List[Scope[Int, Core, V]]) extends Core[V]
  case class LamDict[+V](body: Scope[Unit, Core, V]) extends Core[V]
  case class AppDict[+V](f: Core[V], d: Core[V]) extends Core[V]

trait HardCore extends Core[Nothing]
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
