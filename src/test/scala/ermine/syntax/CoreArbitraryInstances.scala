package ermine.syntax

import org.scalacheck._
import Gen._
import Arbitrary.arbitrary
import bound.scalacheck.BoundArbitraryInstances._
import bound.Scope

object CoreArbitraryInstances {

  implicit val ArbitraryHardCore: Arbitrary[HardCore] = Arbitrary(oneOf(
    arbitrary[Byte]  .map(Super),
    arbitrary[Byte]  .map(Slot),
    arbitrary[Int]   .map(LitInt),
    arbitrary[Int]   .map(LitInt64(_)),
    arbitrary[Byte]  .map(LitByte),
    arbitrary[Short] .map(LitShort),
    arbitrary[String].map(LitString),
    arbitrary[Char]  .map(LitChar),
    arbitrary[Float] .map(LitFloat),
    arbitrary[Double].map(LitDouble),
    arbitrary[String].map(Err)
  ))

  implicit val Arbitrary1Core: Arbitrary1[Core] = new Arbitrary1[Core] {
    def arbitrary1[V](implicit a: Arbitrary[V]): Arbitrary[Core[V]] = implicitly[Arbitrary[Core[V]]]
  }

  implicit def ArbitraryCore[V](implicit av: Arbitrary[V]): Arbitrary[Core[V]] = Arbitrary(Gen.sized { size =>
    def resize[T](g:Gen[T]) = Gen.resize(size / 2, g)
    size match {
      case 0 => oneOf(for { v <- av.arbitrary } yield Var(v), ArbitraryHardCore.arbitrary)
      case n => oneOf(
        //        for { v <- av.arbitrary } yield Var(v),
        ArbitraryHardCore.arbitrary,
        for { tag <- arbitrary[Int]; fields <- resize(arbitrary[List[Core[V]]]) } yield Data(tag, fields),
        for { f <- resize(arbitrary[Core[V]]); x <- resize(arbitrary[Core[V]]) } yield App(f, x),
        for { arity <- arbitrary[Int]; body <- resize(arbitrary[Scope[Int, Core, V]]) } yield Lam(arity, body),
        for { bindings <- resize(arbitrary[List[Scope[Int, Core, V]]]); body <- resize(arbitrary[Scope[Int, Core, V]]) } yield Let(bindings, body),
        for {
          c        <- resize(arbitrary[Core[V]]);
          branches <- resize(arbitrary[Map[Int, Scope[Int, Core, V]]])
          default  <- resize(arbitrary[Option[Scope[Unit, Core, V]]])
        } yield Case(c, branches, default),
        for { supers <- resize(arbitrary[Int]); slots <- resize(arbitrary[List[Core[V]]]) } yield Data(supers, slots),
        for { body <- resize(arbitrary[Scope[Unit, Core, V]]) } yield LamDict(body),
        for { f <- resize(arbitrary[Core[V]]); d <- resize(arbitrary[Core[V]]) } yield AppDict(f, d)
      )
    }
  })
}
