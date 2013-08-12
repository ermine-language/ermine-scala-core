package ermine.syntax

import f0._
import Readers._
import Writers._
import bound._
import bound.BoundSerialization._

/**
 * f0 Readers and Writers for Core.
 */
object CoreSerialization {

  lazy val hardcoreW: Writer[HardCore, DynamicF] = s4W(
    byteW,  // Super
    byteW,  // Slot
    litW,   // Lit
    stringW // Err
  )((a,b,c,d) => (hc: HardCore) => hc match {
    case Super(i) => a(i)
    case Slot(i)  => b(i)
    case l:Lit    => c(l)
    case Err(msg) => d(msg)
  }).erase

  lazy val litW: Writer[Lit, DynamicF] = s8W(
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
  }).erase

  lazy val hardcoreR: Reader[HardCore, DynamicF] = union4R(
    byteR   .map(Super),
    byteR   .map(Slot),
    litR,
    stringR .map(Err)
  ).erase

  lazy val litR: Reader[Lit, DynamicF] = union8R(
    intR    .map(LitInt),
    longR   .map(LitInt64),
    byteR   .map(LitByte),
    shortR  .map(LitShort),
    stringR .map(LitString),
    intR    .map(i => LitChar(i.toChar)),
    floatR  .map(LitFloat),
    doubleR .map(LitDouble)
  ).erase

  def branchesW[V,F](vw: Writer[V,F]): Writer[Map[Byte, Scope[Byte, Core, V]], DynamicF] =
    streamW(tuple2W(byteW, scopeByteCoreVW(vw))).cmap((m:Map[Byte, Scope[Byte, Core, V]]) => m.toList).erase

  def branchesR[V,F](vr: Reader[V,F]): Reader[Map[Byte, Scope[Byte, Core, V]], DynamicF] =
    streamR(tuple2R(byteR, scopeByteCoreVR(vr))).map(_.toMap).erase

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
}
