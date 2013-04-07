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

  val hardcoreW: Writer[HardCore, DynamicF] = s10W(
    intW,                            // Super
    intW,                            // Slot
    intW,                            // LitInt
    longW,                           // LitInt64
    byteW,                           // LitByte
    shortW,                          // LitShort
    stringW,                         // LitString
    intW.cmap((c:Char) => c.toInt),  // LitChar
    floatW,                          // LitFloat
    doubleW                          // LitDouble
  )((a,b,c,d,e,f,g,h,i,j) => (hc: HardCore) => hc match {
    case Super(i)     => a(i)
    case Slot(i)      => b(i)
    case LitInt(i)    => c(i)
    case LitInt64(l)  => d(l)
    case LitByte(b)   => e(b)
    case LitShort(s)  => f(s)
    case LitString(s) => g(s)
    case LitChar(c)   => h(c)
    case LitFloat(f)  => i(f)
    case LitDouble(d) => j(d)
  }).erase

  val hardcoreR: Reader[HardCore, DynamicF] = union10R(
    intR    .map(Super(_)),
    intR    .map(Slot(_)),
    intR    .map(LitInt(_)),
    longR   .map(LitInt64(_)),
    byteR   .map(LitByte(_)),
    shortR  .map(LitShort(_)),
    stringR .map(LitString(_)),
    intR    .map(i => LitChar(i.toChar)),
    floatR  .map(LitFloat(_)),
    doubleR .map(LitDouble(_))
  ).erase

  def branchW[V](vw: Writer[V, DynamicF]): Writer[Branch[V], DynamicF] = s2W(
    tuple2W(intW, scopeIntCoreVW(vw)),  // Labeled
    scopeIntCoreVW(vw)                  // Default
  )((labelWriter, defaultWriter) => (branch: Branch[V]) => branch match {
    case Labeled(tag, body) => labelWriter((tag, body))
    case Default(     body) => defaultWriter(body)
  }).erase

  def branchR[V](vr: Reader[V, DynamicF]): Reader[Branch[V], DynamicF] = union2R(
    tuple2R(intR, scopeIntCoreVR(vr)).map(t => Labeled(t._1, t._2)),
    scopeIntCoreVR(vr).map(Default(_))
  ).erase

  val coreW1: Writer1[Core] = new Writer1[Core] {
    def apply[A](aw: Writer[A, DynamicF]): Writer[Core[A], DynamicF] = coreW(aw)
  }

  val coreR1: Reader1[Core] = new Reader1[Core] {
    def apply[A](aw: Reader[A, DynamicF]): Reader[Core[A], DynamicF] = coreR(aw)
  }

  def scopeIntCoreVW[V] (vw: Writer[V, DynamicF]): Writer[Scope[Int, Core, V], DynamicF] = scopeW(intW, coreW1, vw)
  def scopeIntCoreVR[V] (vr: Reader[V, DynamicF]): Reader[Scope[Int, Core, V], DynamicF] = scopeR(intR, coreR1, vr)

  type CoreF[V] = DynamicF

  def coreW[V](vw: Writer[V, DynamicF]): Writer[Core[V], CoreF[V]] = {
    val si = scopeIntCoreVW(vw)
    fixFW[Core[V],CoreF](self => s10W(
      vw,                                  // Var
      hardcoreW,                           // HardCore
      tuple2W(intW, repeatW(self)),        // Data
      tuple2W(self, self),                 // App
      tuple2W(intW, si),                   // Lam
      tuple2W(repeatW(si), si),            // Let
      tuple2W(self, repeatW(branchW(vw))), // Case
      tuple2W(repeatW(self), repeatW(si)), // Dict
      scopeW(unitW, coreW1, vw),           // LamDict
      tuple2W(self, self)                  // AppDict
    )((a,b,c,d,e,f,g,h,i,j)  => (core: Core[V]) => core match {
      case Var(v)            => a(v)
      case h:HardCore        => b(h)
      case Data(t, fs)       => c((t, fs))
      case App(f, x)         => d((f, x))
      case Lam(ar, body)     => e((ar, body))
      case Let(bs, b)        => f((bs, b))
      case Case(c, bs)       => g((c, bs))
      case Dict(sups, slots) => h((sups, slots))
      case LamDict(b)        => i(b)
      case AppDict(f, d)     => j((f, d))
    }).erase).erase
  }

  def coreR[V](vr: Reader[V, DynamicF]): Reader[Core[V], CoreF[V]] = {
    val si = scopeIntCoreVR(vr)
    fixFR[Core[V], CoreF](self =>
      union10R(
        vr.map(Var(_)),
        hardcoreR,
        tuple2R(intR, listR(coreR(vr)))        .map(t => Data(t._1, t._2)),
        tuple2R(coreR(vr), coreR(vr))          .map(t => App(t._1, t._2)),
        tuple2R(intR, si)                      .map(t => Lam(t._1, t._2)),
        tuple2R(listR(si), si)                 .map(t => Let(t._1, t._2)),
        tuple2R(coreR(vr), listR(branchR(vr))) .map(t => Case(t._1, t._2)),
        tuple2R(listR(coreR(vr)), listR(si))   .map(t => Dict(t._1, t._2)),
        scopeR(unitR, coreR1, vr)              .map(t => LamDict(t)),
        tuple2R(coreR(vr), coreR(vr))          .map(t => AppDict(t._1, t._2))
      ).erase
    ).erase
  }
}

