package ermine.syntax

import f0._
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

  def branchW[V](vw: Writer[V, DynamicF]): Writer[Branch[V], DynamicF] = s2W(
    tuple2W(intW, scopeIntCoreVW(vw)),  // Labeled
    scopeIntCoreVW(vw)                  // Default
  )((labelWriter, defaultWriter) => (branch: Branch[V]) => branch match {
    case Labeled(tag, body) => labelWriter((tag, body))
    case Default(     body) => defaultWriter(body)
  }).erase

  val coreW1: Writer1[Core] = new Writer1[Core] {
    def apply[A](aw: Writer[A, DynamicF]): Writer[Core[A], DynamicF] = coreW(aw)
  }

  def scopeIntCoreVW[V] (vw: Writer[V, DynamicF]): Writer[Scope[Int,  Core, V], DynamicF] = scopeW(intW, coreW1, vw)

  def coreW[V](vw: Writer[V, DynamicF]): Writer[Core[V], DynamicF] = {
    val si = scopeIntCoreVW(vw)
    s10W(
      vw,                                       // Var
      hardcoreW,                                // HardCore
      tuple2W(intW, repeatW(coreW(vw))),        // Data
      tuple2W(coreW(vw), coreW(vw)),            // App
      tuple2W(intW, si),                        // Lam
      tuple2W(repeatW(si), si),                 // Let
      tuple2W(coreW(vw), repeatW(branchW(vw))), // Case
      tuple2W(repeatW(coreW(vw)), repeatW(si)), // Dict
      scopeW(unitW, coreW1, vw),                // LamDict
      tuple2W(coreW(vw), coreW(vw))             // AppDict
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
    }).erase
  }
}

