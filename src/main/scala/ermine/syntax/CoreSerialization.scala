package ermine.syntax

import f0._
import Readers._
import Writers._
import bound._
import bound.BoundSerialization._

object CoreSerialization {
  val hardcoreW: Writer[HardCore, DynamicF] = s10W(
    intW,
    intW,
    intW,
    longW,
    byteW,
    shortW,
    stringW,
    intW.cmap((c:Char) => c.toInt),
    floatW,
    doubleW
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

  val hardcoreR: Reader[HardCore, DynamicF] = s10R(
    intR,
    intR,
    intR,
    longR,
    byteR,
    shortR,
    stringR,
    intR.map(_.toChar),
    floatR,
    doubleR
  )(
    Super(_),
    Slot(_),
    LitInt(_),
    LitInt64(_),
    LitByte(_),
    LitShort(_),
    LitString(_),
    LitChar(_),
    LitFloat(_),
    LitDouble(_)
  ).erase

  def branchW[V](vw: Writer[V, DynamicF]): Writer[Branch[V], DynamicF] = s2W(
    tuple2W(intW, scopeIntCoreVW(vw)),  // Labeled
    scopeIntCoreVW(vw)                  // Default
  )((labelWriter, defaultWriter) => (branch: Branch[V]) => branch match {
    case Labeled(tag, body) => labelWriter((tag, body))
    case Default(     body) => defaultWriter(body)
  }).erase

  def branchR[V](vr: Reader[V, DynamicF]): Reader[Branch[V], DynamicF] = s2R(
    tuple2R(intR, scopeIntCoreVR(vr)),  // Labeled
    scopeIntCoreVR(vr)                  // Default
  )(t => Labeled(t._1, t._2), Default(_)).erase

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
      s10R(
        vr,                                     // Var
        hardcoreR,                              // HardCore
        tuple2R(intR, listR(coreR(vr))),        // Data
        tuple2R(coreR(vr), coreR(vr)),          // App
        tuple2R(intR, si),                      // Lam
        tuple2R(listR(si), si),                 // Let
        tuple2R(coreR(vr), listR(branchR(vr))), // Case
        tuple2R(listR(coreR(vr)), listR(si)),   // Dict
        scopeR(unitR, coreR1, vr),              // LamDict
        tuple2R(coreR(vr), coreR(vr))           // AppDict
      )(
        Var(_),
        (h: HardCore) => h,
        t => Data(t._1, t._2),
        t => App(t._1, t._2),
        t => Lam(t._1, t._2),
        t => Let(t._1, t._2),
        t => Case(t._1, t._2),
        t => Dict(t._1, t._2),
        LamDict(_),
        t => AppDict(t._1, t._2)
      ).erase
    ).erase
  }
}

