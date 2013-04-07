package ermine.syntax

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import bound.scalacheck.BoundArbitraryInstances._
import bound.Scope

class CoreArbitraryInstances {

  implicit val ArbitraryHardCore: Arbitrary[HardCore] = Arbitrary(oneOf(
    arbitrary[Int]   .map(Super(_)),
    arbitrary[Int]   .map(Slot(_)),
    arbitrary[Int]   .map(LitInt(_)),
    arbitrary[Int]   .map(LitInt64(_)),
    arbitrary[Byte]  .map(LitByte(_)),
    arbitrary[Short] .map(LitShort(_)),
    arbitrary[String].map(LitString(_)),
    arbitrary[Char]  .map(LitChar(_)),
    arbitrary[Float] .map(LitFloat(_)),
    arbitrary[Double].map(LitDouble(_))
  ))

  implicit def ArbitraryBranch[V](implicit a: Arbitrary[V]): Arbitrary[Branch[V]] = Arbitrary(oneOf(
    for { i <- arbitrary[Int]; s <- arbitrary[Scope[Int, Core, V]] } yield Labeled(i, s),
    arbitrary[Scope[Int, Core, V]].map(Default(_)))
  )

  implicit val Arbitrary1Core: Arbitrary1[Core] = new Arbitrary1[Core] {
    def arbitrary1[V](implicit a: Arbitrary[V]): Arbitrary[Core[V]] = implicitly[Arbitrary[Core[V]]]
  }

  implicit def ArbitraryCore[V](implicit av: Arbitrary[V]): Arbitrary[Core[V]] = Arbitrary(oneOf(
    for { v <- av.arbitrary } yield Var(v),
    ArbitraryHardCore.arbitrary,
    for { tag <- arbitrary[Int]; fields <- arbitrary[List[Core[V]]] } yield Data(tag, fields),
    for { f <- arbitrary[Core[V]]; x <- arbitrary[Core[V]] } yield App(f, x),
    for { arity <- arbitrary[Int]; body <- arbitrary[Scope[Int, Core, V]] } yield Lam(arity, body),
    for { bindings <- arbitrary[List[Scope[Int, Core, V]]]; body <- arbitrary[Scope[Int, Core, V]] } yield Let(bindings, body),
    for { c <- arbitrary[Core[V]]; branches <- arbitrary[List[Branch[V]]] } yield Case(c, branches),
    for { supers <- arbitrary[Int]; slots <- arbitrary[List[Core[V]]] } yield Data(supers, slots),
    for { body <- arbitrary[Scope[Unit, Core, V]] } yield LamDict(body),
    for { f <- arbitrary[Core[V]]; d <- arbitrary[Core[V]] } yield AppDict(f, d)
  ))
}
