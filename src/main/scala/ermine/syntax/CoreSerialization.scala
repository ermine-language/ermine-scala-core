package ermine.syntax

import f0._
import Formats._
import Readers._
import Writers._
import bound._
import bound.BoundSerialization._

/**
 * f0 Readers and Writers for Core.
 */
object CoreSerialization {

  type LitF = S8[IntF, LongF, ByteF, ShortF, StringF, IntF, FloatF, DoubleF]
  lazy val litW: Writer[Lit, LitF] = s8W(
    intW,                            // LitInt
    longW,                           // LitInt64
    byteW,                           // LitByte
    shortW,                          // LitShort
    stringW,                         // LitString
    intW.cmap((c:Char) => c.toInt),  // LitChar
    floatW,                          // LitFloat
    doubleW                          // LitDouble
  )((a,b,c,d,e,f,g,h) => (l: Lit) => l match {
    case LitInt(i)    => a(i)
    case LitInt64(l)  => b(l)
    case LitByte(b)   => c(b)
    case LitShort(s)  => d(s)
    case LitString(s) => e(s)
    case LitChar(c)   => f(c)
    case LitFloat(f)  => g(f)
    case LitDouble(d) => h(d)
  })

  lazy val litR: Reader[Lit, LitF] = union8R(
    intR    .map(LitInt),
    longR   .map(LitInt64),
    byteR   .map(LitByte),
    shortR  .map(LitShort),
    stringR .map(LitString),
    intR    .map(i => LitChar(i.toChar)),
    floatR  .map(LitFloat),
    doubleR .map(LitDouble)
  )

  type MethodF         = BooleanF :: StringF :: StringF :: StreamF[StringF]
  type ConstructorF    = StringF :: StreamF[StringF]
  type ValueF          = BooleanF :: StringF :: StringF
  type ForeignF        = S3[MethodF, ConstructorF, ValueF]

  lazy val foreignW: Writer[Foreign, ForeignF] = s3W(
    tuple4W(booleanW, stringW, stringW, streamW(stringW)), // ForeignMethod
    tuple2W(stringW, streamW(stringW)),                    // ForeignConstructor
    tuple3W(booleanW, stringW, stringW)                    // ForeignValue
  )((a,b,c) => (f: Foreign) => f match {
    case ForeignMethod(s, cn, mn, args) => a((s, cn, mn, args))
    case ForeignConstructor(cn, args)   => b((cn, args))
    case ForeignValue(s, cn, fn)        => c((s, cn, fn))
  })

  lazy val foreignR: Reader[Foreign, ForeignF] = union3R(
    tuple4R(booleanR, stringR, stringR, streamR(stringR)).map{ case (s, cn, mn, args) => ForeignMethod(s, cn, mn, args)},
    tuple2R(stringR, streamR(stringR))                   .map{ case (cn, args)        => ForeignConstructor(cn, args)},
    tuple3R(booleanR, stringR, stringR)                  .map{ case (s, cn, fn)       => ForeignValue(s, cn, fn)}
  )

  /**
   * Haskell Foreigns have slightly more structure than Foreigns in Scala.
   * This is because the Haskell version supports other languages that Scala
   * does not need to know about. JavaLike comes first in Haskell serialization, like so:
   *
   * instance Serial Foreign where
   *   serialize (JavaLike j) = putWord8 0 >> serialize j
   *   serialize (Unknown s)  = ...
   *
   * Therefore, Scala must add an extra 0:Byte when writing out Foreigns to Haskell,
   * and when reading, must read in a byte before reading the Foreign. If that byte is anything
   * other than 0, then Haskell must have given us some bad (non-jvm language) information.
   */
  type ForeignFHaskell = ByteF :: ForeignF
  lazy val foreignWHaskell: Writer[Foreign, ForeignFHaskell] = tuple2W(byteW, foreignW).cmap((f: Foreign) => (0, f))
  lazy val foreignRHaskell: Reader[Foreign, ForeignFHaskell] = tuple2R(byteR, foreignR).map{
    case (0, f) => f
    case (_, f) => sys.error("non javalike foreign detected.")
  }

  type HardcoreF = S8[ByteF, ByteF, LitF, StringF, ForeignFHaskell, StringF ,GlobalF, DigestF]
  lazy val hardcoreW: Writer[HardCore, HardcoreF] = s8W(
    byteW,   // Super
    byteW,   // Slot
    litW,    // Lit
    stringW, // PrimOp
    foreignWHaskell, // Foreign
    stringW, // Err
    globalW, // GlobalRef
    digestW  // InstanceRef
  )((a,b,c,d,e,f,g,h) => (hc: HardCore) => hc match {
    case Super(i)         => a(i)
    case Slot(i)          => b(i)
    case l:Lit            => c(l)
    case PrimOp(n)        => d(n)
    case f: Foreign       => e(f)
    case Err(msg)         => f(msg)
    case GlobalRef(gl)    => g(gl)
    case InstanceRef(dig) => h(dig)
  })

  lazy val hardcoreR: Reader[HardCore, HardcoreF] = union8R(
    byteR   .map(Super),
    byteR   .map(Slot),
    litR,
    stringR .map(PrimOp),
    foreignRHaskell,
    stringR .map(Err),
    globalR .map(GlobalRef),
    digestR .map(InstanceRef)
  )

  def branchesW[V,F](vw: Writer[V,F]): Writer[Map[Byte, (Byte, Scope[Byte, Core, V])], DynamicF] =
    streamW(tuple2W(byteW, tuple2W(byteW, scopeByteCoreVW(vw)))).cmap((m:Map[Byte, (Byte, Scope[Byte, Core, V])]) => m.toList).erase

  def branchesR[V,F](vr: Reader[V,F]): Reader[Map[Byte, (Byte, Scope[Byte, Core, V])], DynamicF] =
    streamR(tuple2R(byteR, tuple2R(byteR, scopeByteCoreVR(vr)))).map(_.toMap).erase

  val coreW1: Writer1[Core] = new Writer1[Core] {
    def apply[A](aw: Writer[A, DynamicF]): Writer[Core[A], DynamicF] = coreW(aw)
  }

  val coreR1: Reader1[Core] = new Reader1[Core] {
    def apply[A](aw: Reader[A, DynamicF]): Reader[Core[A], DynamicF] = coreR(aw)
  }

  def scopeByteCoreVW[V,F] (vw: Writer[V, F]): Writer[Scope[Byte, Core, V], DynamicF] = scopeW(byteW, coreW1, vw)
  def scopeByteCoreVR[V,F] (vr: Reader[V, F]): Reader[Scope[Byte, Core, V], DynamicF] = scopeR(byteR, coreR1, vr)

  def scopeUnitCoreVW[V,F] (vw: Writer[V, F]): Writer[Scope[Unit, Core, V], DynamicF] = scopeW(unitW, coreW1, vw)
  def scopeUnitCoreVR[V,F] (vr: Reader[V, F]): Reader[Scope[Unit, Core, V], DynamicF] = scopeR(unitR, coreR1, vr)

  type CoreF[V] = DynamicF

  def coreW[V,F](vw: Writer[V, F]): Writer[Core[V], CoreF[V]] = {
    def sb = scopeByteCoreVW(vw)
    def su = scopeUnitCoreVW(vw)
    fixFW[Core[V],CoreF](self => s10W(
      vw,                                                 // Var
      hardcoreW,                                          // HardCore
      tuple2W(byteW, streamW(self)),                      // Data
      tuple2W(self, self),                                // App
      tuple2W(byteW, sb),                                 // Lam
      tuple2W(streamW(sb), sb),                           // Let
      tuple3W(self, branchesW(vw), optionW(su)),          // Case
      tuple2W(streamW(self), streamW(sb)),                // Dict
      su,                                                 // LamDict
      tuple2W(self, self)                                 // AppDict
    )((a,b,c,d,e,f,g,h,i,j)  => (core: Core[V]) => core match {
      case Var(v)            => a(v)
      case h:HardCore        => b(h)
      case Data(t, fs)       => c((t, fs))
      case App(f, x)         => d((f, x))
      case Lam(ar, body)     => e((ar, body))
      case Let(bs, b)        => f((bs, b))
      case Case(c, bs, d)    => g((c, bs, d))
      case Dict(sups, slots) => h((sups, slots))
      case LamDict(b)        => i(b)
      case AppDict(f, d)     => j((f, d))
    }).erase).erase
  }

  def coreR[V,F](vr: Reader[V, F]): Reader[Core[V], CoreF[V]] = {
    def sb = scopeByteCoreVR(vr)
    def su = scopeUnitCoreVR(vr)
    fixFR[Core[V], CoreF](self =>
      union10R(
        vr.map(Var(_)),
        hardcoreR,
        tuple2R(byteR, streamR(coreR(vr)))      .map(t => Data(t._1, t._2)),
        tuple2R(coreR(vr), coreR(vr))           .map(t => App(t._1, t._2)),
        tuple2R(byteR, sb)                      .map(t => Lam(t._1, t._2)),
        tuple2R(streamR(sb), sb)                .map(t => Let(t._1, t._2)),
        tuple3R(coreR(vr), branchesR(vr), optionR(su)).map(t => Case(t._1, t._2, t._3)),
        tuple2R(streamR(coreR(vr)), streamR(sb)).map(t => Dict(t._1, t._2)),
        su                                      .map(t => LamDict(t)),
        tuple2R(coreR(vr), coreR(vr))           .map(t => AppDict(t._1, t._2))
      ).erase
    ).erase
  }

  type AssocF = ByteF
  lazy val assocW: Writer[Assoc, AssocF] = byteW.cmap((a: Assoc) => a match {
    case L => 0:Byte
    case R => 1:Byte
    case N => 2:Byte
  })

  lazy val assocR: Reader[Assoc, AssocF] = byteR.map((b: Byte) => b match {
    case 0 => L
    case 1 => R
    case 2 => N
  })

  type FixityF = S4[P2[AssocF, IntF], IntF, IntF, UnitF]
  lazy val fixityW: Writer[Fixity, FixityF] = s4W(
    tuple2W(assocW, intW), // Infix
    intW,                  // Prefix
    intW,                  // Postfix
    unitW                  // Idfix
  )((a,b,c,d) => (f: Fixity) => f match {
    case Infix(assoc, l) => a((assoc, l))
    case Prefix(i)       => b(i)
    case Postfix(i)      => c(i)
    case Idfix           => d(())
  })

  lazy val fixityR: Reader[Fixity, FixityF] = union4R(
    tuple2R(assocR, intR).map{ case (a, l) => Infix(a, l) },
    intR .map(Prefix),
    intR .map(Postfix),
    unitR.map(_ => Idfix)
  )

  type DigestF = LongF :: LongF

  val digestW: Writer[Digest, DigestF] =
    tuple2W(longW, longW).cmap((d: Digest) => (d.part1, d.part2))

  val digestR: Reader[Digest, DigestF] =
    tuple2R(longR, longR).map{ case (p1, p2) => new Digest(p1, p2) }

  type ModuleNameF = DigestF :: StringF :: StringF

  val moduleNameW: Writer[ModuleName, ModuleNameF] =
    tuple3W(digestW, stringW, stringW).cmap((m: ModuleName) => (m.digest, m.pkg, m.name))

  val moduleNameR: Reader[ModuleName, ModuleNameF] =
    tuple3R(digestR, stringR, stringR).map{ case (d, p, n) => ModuleName(d, p, n) }

  type GlobalF = DigestF :: FixityF :: ModuleNameF :: StringF

  val globalW: Writer[Global, GlobalF] =
    tuple4W(digestW, fixityW, moduleNameW, stringW).cmap((g: Global) => (g.digest, g.fixity, g.module, g.name))

  val globalR: Reader[Global, GlobalF] =
    tuple4R(digestR, fixityR, moduleNameR, stringR).map{ case (d, f, m, n) => Global(d, f, m, n) }

  type TermExportsF[F] = StreamF[P2[GlobalF, S2[GlobalF, F]]]
  def termExportsW[V, F](vw: Writer[V, F]): Writer[Map[Global, Either[Global, V]], TermExportsF[F]] =
    streamW(tuple2W(globalW, eitherW(globalW, vw)))

  def termExportsR[V, F](vr: Reader[V, F]): Reader[Map[Global, Either[Global, V]], TermExportsF[F]] =
    streamR(tuple2R(globalR, eitherR(globalR, vr))).map(_.toMap)

  type ModuleF[F] = ModuleNameF :: StreamF[CoreF[F]] :: TermExportsF[F] :: StreamF[P2[DigestF, F]]

  def moduleW[V, F](vw: Writer[V, F]): Writer[Module[V], ModuleF[F]] = tuple4W(
    moduleNameW, streamW(coreW(vw)), termExportsW(vw), streamW(tuple2W(digestW, vw))
  ).cmap((m: Module[V]) => (m.name, m.definitions, m.termExports, m.instances.toStream))

  def moduleR[V, F](vr: Reader[V, F]): Reader[Module[V], ModuleF[F]] = tuple4R(
    moduleNameR, streamR(coreR(vr)), termExportsR(vr), streamR(tuple2R(digestR, vr))
  ).map{ case (m, defs, terms, insts) => Module(m, defs.toVector, terms, insts.toMap) }

  /**
   * When Haskell reads in a Module, it expects some extra information that Scala doesn't know about.
   *
   *   _instances   :: Map ByteString Int,
   *   _types       :: Map Global (Type Void Void),
   *   _data        :: [DataType Void Void]
   *
   * When writing to Haskell, we supply empty values for these fields.
   *
   * When reading from Haskell (or from files written by the compiler), we can simply stop
   * reading after reading the first four fields of the module. This strategy will fail
   * if we have to read a list of modules, but I believe that we will be reading Modules
   * individually from files, so this should be ok. If it isn't we may have to tweek the
   * way Haskell serializes Modules. One possibility is the include the length of the
   * information remaining, so that we can skip over it.
   */
  type ModuleHaskell[F] = ModuleF[F] :: StreamF[UnitF] :: StreamF[UnitF] :: StreamF[UnitF]
}
