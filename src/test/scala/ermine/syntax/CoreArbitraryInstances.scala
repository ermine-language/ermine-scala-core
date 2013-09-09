package ermine.syntax

import org.scalacheck._
import Shrink._
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
    def resize[T](g:Gen[T]) = Gen.resize(size / 4, g)
    size match {
      case 0 => oneOf(for { v <- av.arbitrary } yield Var(v), ArbitraryHardCore.arbitrary)
      case n => oneOf(
        for { v <- av.arbitrary } yield Var(v),
        ArbitraryHardCore.arbitrary,
        for {
          tag    <- arbitrary[Byte]
          fields <- resize(arbitrary[List[Core[V]]])
        } yield Data(tag, fields),
        for {
          f <- resize(arbitrary[Core[V]])
          x <- resize(arbitrary[Core[V]])
        } yield App(f, x),
        for {
          arity <- arbitrary[Byte]
          body  <- resize(arbitrary[Scope[Byte, Core, V]])
        } yield Lam(arity, body),
        for {
          bindings <- resize(arbitrary[List[Scope[Byte, Core, V]]])
          body     <- resize(arbitrary[Scope[Byte, Core, V]])
        } yield Let(bindings, body),
        for {
          c        <- resize(arbitrary[Core[V]])
          branches <- resize(arbitrary[Map[Byte, (Byte, Scope[Byte, Core, V])]])
          default  <- resize(arbitrary[Option[Scope[Unit, Core, V]]])
        } yield Case(c, branches, default),
        for {
          supers <- resize(arbitrary[Byte])
          slots  <- resize(arbitrary[List[Core[V]]])
        } yield Data(supers, slots),
        for { body <- resize(arbitrary[Scope[Unit, Core, V]]) } yield LamDict(body),
        for {
          f <- resize(arbitrary[Core[V]])
          d <- resize(arbitrary[Core[V]])
        } yield AppDict(f, d)
      )
    }
  })

  //def fromScope[F[+_], A, B](s : bound.Scope[B, F, A])(implicit evidence$20 : scalaz.Monad[F]) : F[bound.Var[B, A]]
  //import bound._
  //import Core._
  implicit def shrinkCore[V](implicit s1: Shrink[V]): Shrink[Core[V]] = Shrink { c =>
    c match {
      case Var(_)               => Stream.empty
      case h: HardCore          => Stream.empty
      case App(f, x)            => Stream(f, x)
      // case class Let[+V](bindings: List[Scope[Byte, Core, V]], body: Scope[Byte, Core, V])
      case Let(bindings, body)  => Stream() // TODO: bindings, body
      // case class Case[+V](c: Core[V], branches: Map[Byte, (Byte, Scope[Byte, Core, V])], default: Option[Scope[Unit, Core, V]])
      case Case(c, branches, d) => Stream(c) // TODO: branches, default
      case Data(tag, fields)    => fields.toStream
      // case class LamDict[+V](body: Scope[Unit, Core, V])
      case LamDict(body)        => Stream() // TODO: body
      case AppDict(f, d)        => Stream(f, d)
    }
  }

  implicit val ArbitraryAssoc: Arbitrary[Assoc] = Arbitrary(oneOf(L, R, N))

  implicit val ArbitraryFixity: Arbitrary[Fixity] = Arbitrary(oneOf(
    for { a <- arbitrary[Assoc]; l <- arbitrary[Int] } yield Infix(a, l),
    arbitrary[Int]  .map(Prefix),
    arbitrary[Int]  .map(Postfix),
    Idfix
  ))

  implicit val ArbitraryDigest: Arbitrary[Digest] = Arbitrary(
    for { p1 <- arbitrary[Long]; p2 <- arbitrary[Long] } yield new Digest(p1, p2)
  )

  implicit val ArbitraryModuleName: Arbitrary[ModuleName] = Arbitrary(
    for { pkg <- arbitrary[String]; n <- arbitrary[String] } yield ModuleName(pkg, n)
  )

  implicit val ArbitraryGlobal: Arbitrary[Global] = Arbitrary(
    for { mn <- arbitrary[ModuleName]; n  <- arbitrary[String]; f <- arbitrary[Fixity] } yield Global(mn, n, f)
  )

  implicit def ArbitraryModule[V](implicit av: Arbitrary[V]): Arbitrary[Module[V]] = Arbitrary(Gen.sized { size =>
    def resize[T](g:Gen[T]) = Gen.resize(size / 4, g)
    for { mn    <- arbitrary[ModuleName]
          defs  <- resize(arbitrary[List[Core[V]]])
          terms <- arbitrary[Map[Global, Either[Global, V]]]
          insts <- arbitrary[Map[Digest, V]]
    } yield Module(mn, defs.toVector, terms, insts)
  })

  implicit def shrinkModule[V](implicit s1: Shrink[V]): Shrink[Module[V]] = Shrink {
    case Module(name, defs, terms, insts) =>
      Stream(Module(name, Vector(), Map(), Map())) append
      (for(ds <- shrink(defs))  yield Module(name, ds,       Map(), Map())) append
      (for(ts <- shrink(terms)) yield Module(name, Vector(), ts,    Map())) append
      (for(is <- shrink(insts)) yield Module(name, Vector(), Map(), is))
  }

}
