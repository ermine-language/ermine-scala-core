package ermine.eval

import bound.Scope
import ermine.syntax.{Data => CoreData, _}

/**
 * A type of addresses, to be used as unique identifiers in an environment map.
 */
class Address

import Eval.Env

/**
 * Common supertype of all runtime values.
 */
sealed abstract class Runtime

/**
 * This constructor may be used to represent both compiled lambda
 * expressions and foreign imported functions (the latter must be
 * appropriately wrapped, of course). Functions may take multiple
 * arguments at a time, and are able to report their arity.
 */
case class Func(arity: Byte, body: List[Runtime] => Runtime) extends Runtime

/**
 * Since function arity is not necessarily 1, we may encounter a situation
 * in which we don't have enough arguments to call a function yet. This
 * represents a suspended application for such a situation.
 */
case class PartialApp(func: Runtime, args: List[Runtime]) extends Runtime

/**
 * Representation of data type constructors.
 */
case class Data(tag: Byte, fields: List[Runtime]) extends Runtime

case class Evidence(supers: List[Runtime], slots: List[Runtime]) extends Runtime

/**
 * Detectable non-termination, such as exceptions.
 */
class Bottom(msg: => Nothing) extends Runtime {
  def inspect = msg
}

object Bottom {
  def apply(msg: => Nothing): Bottom = new Bottom(msg)
  def unapply(f: Bottom): Option[() => Nothing] = Some(() => f.inspect)
}

abstract class ThunkState
class Delayed(env: => Env, val core: Core[Address]) extends ThunkState {
  def getEnv = env
}

object Delayed {
  def apply(env: => Env, e: Core[Address]) = new Delayed(env, e)
  def unapply(e: ThunkState): Option[(Env, Core[Address])] = e match {
    case d : Delayed => Some((d.getEnv, d.core))
    case _           => None
  }
}

/**
 * Black holes are used to temporarily replace thunk states during
 * evaluation. If a thunk with a blackhole is subsequently encountered
 * during evaluation, then we have encountered a viscious circularity,
 * which can be reported with an exception.
 *
 * This thunk state is temporary, for an initial pass interpreter. If
 * multithreaded evaluation is desired (and it is), one must use white
 * holes, which cause evaluation to block, rather than die. Loop detection
 * can be maintained by recording thread identifiers, however.
 */
case object BlackHole extends ThunkState
case class Evaluated(e: Runtime) extends ThunkState

/**
 * A delayed value. Internally stores an updatable state, initially
 * referencing the information needed to compute a value, but which may be
 * replaced on demand by the value to be computed itself.
 *
 * Each thunk also carries a boolean which states whether or not
 * writebacks should be performed at all, for situations in which sharing
 * is unnecessary.
 */
class Thunk(var state: ThunkState, val update: Boolean) extends Runtime {
  def render = Eval.whnf(this).render
}

object Thunk {
  def apply(env: => Env, e: Core[Address], update: Boolean) =
    new Thunk(Delayed(env, e), update)
}

case class Prim(p: Any) extends Runtime {
  def render = toString
}


object Eval {
  type Env = Map[Address, Runtime]

  def die(msg: String) = throw Death(msg)

  @annotation.tailrec
  final def eval(env: Env, core: Core[Address], stk: List[Runtime] = Nil): Runtime = core match {
    case Var(a)            => appl(env.getOrElse(a, panic(s"bad variable reference $a")), stk)
    case Super(b)          => stk match {
      case Evidence(sups, _) :: rest => appl(sups(b.toInt), rest)
      case x :: _ => panic(s"Super applied to non-dictionary $x")
      case Nil => Func(1, { case List(Evidence(sups, _)) => sups(b.toInt) })
    }
    case Slot(b)          => stk match {
      case Evidence(_, slots) :: rest => appl(slots(b.toInt), rest)
      case (x :: _) => panic(s"Slot applied to non-dictionary $x")
      case Nil => Func(1, { case List(Evidence(_, slots)) => slots(b.toInt) })
    }
    case Err(msg)          => Bottom(die(msg))
    case l: Lit            => appl(Prim(l.extract), stk)
    case CoreData(tag, cs) => appl(Data(tag, cs.map(Thunk(env, _, true))), stk)
    case App(x, y)         => eval(env, x, Thunk(env, y, true) :: stk)
    case Lam(n, e)         =>
      // this case doesn't use java stack
      if(stk.length >= n) stk.splitAt(n.toInt) match {
        case (args, rest) =>
          val addrs = args.map(_ => new Address)
          val newEnv = addrs.zip(args).toMap ++ env
          eval(newEnv, e.instantiate(b => Var(addrs(b.toInt))), rest)
      }
      // TODO: this case does...
      else appl(Func(n, evalLam(env, e)(_.toInt)), stk)
    case Let(bs, e)     =>
      var newEnv : Env = null
      val addrs = bs.map(_ => new Address)
      val ibs = bs.map(b => b.instantiate(i => Var(addrs(i))))
      newEnv = env ++ addrs.zip(ibs).map { case (addr, b) => addr -> Thunk(newEnv, b, true) }
      eval(newEnv, e.instantiate(i => Var(addrs(i))), stk)
    case Case(e, bs, d) =>
      val (body, aug) = pickBranch(env, e, bs, d)
      eval(env ++ aug, body, stk)
    case Dict(xs, ys)   => appl(buildDict(env, xs, ys), stk)
    case LamDict(e)     => stk match {
      case Nil => Func(1, evalLam(env, e)(_ => 0))
      case dict :: rest =>
        val addr = new Address
        eval(env + (addr -> dict), e.instantiate(_ => Var(addr)), rest)
    }
    case AppDict(x, y)  =>  eval(env, x, evalDict(env, y) :: stk)
    case _ => ???
  }

  def buildDict(env: Env, supers: List[Core[Address]], slots: List[Scope[Byte, Core, Address]]) = {
    val esupers = supers.map(eval(env, _))
    var newEnv : Env = null
    val slotAddrs = slots.map(_ => new Address)
    val eslots = slots.map(b => Thunk(newEnv, b.instantiate(i => Var(slotAddrs(i))), true))
    newEnv = env ++ slotAddrs.zip(eslots).toMap
    Evidence(esupers, eslots)
  }

  def evalDict(env: Env, y: Core[Address]) = eval(env, y)

  private def evalLam[T](env: Env, s:Scope[T, Core, Address])(f: T => Int): List[Runtime] => Runtime = l => {
    val addrs = l.map(_ => new Address)
    val newEnv = addrs.zip(l).toMap ++ env
    eval(newEnv, s.instantiate(b => Var(addrs(f(b)))))
  }

  private def pickBranch(env: Env, c: Core[Address],
                         branches: Map[Byte, (Byte, Scope[Byte, Core, Address])],
                         default: Option[Scope[Unit, Core, Address]]): (Core[Address], Env) = whnf(eval(env, c)) match {
    case d@Data(tag, fields) =>
      val addr = new Address
      branches.get(tag) match {
        case Some((_, body)) =>
          val addrs = fields.map(_ => new Address)
          (body.instantiate(b => Var((addr :: addrs)(b.toInt))), ((addr,d) :: addrs.zip(fields)).toMap)
        case None => default match {
          case Some(body) => (body.instantiate(_ => Var(addr)), Map(addr -> d))
          case None       => panic("non-exhaustive case statement without default")
        }
      }
    case _ => ???
  }

  @annotation.tailrec
  private def appl(v: Runtime, stk: List[Runtime]): Runtime = stk match {
    case Nil => v
    case a :: stkp => whnf(v) match {
      case t : Thunk                   => sys.error("PANIC: whnf returned thunk")
      case b : Bottom                  => b
      case Func(arity, f)              =>
        if(stk.length < arity) PartialApp(v, stk)
        else {
          val (args, rest) = stk.splitAt(arity)
          appl(f(args), rest)
        }
      case PartialApp(f, args)         => appl(f, args ++ stk)
      case Prim(f : Function1[Any, _]) => ??? // appl(Prim(f(a.extract)), stkp)
      case f                           => panic(s"Cannot apply runtime value $f to $a")
    }
  }

  @annotation.tailrec
  final def whnf(r: Runtime, chain: List[Thunk] = Nil): Runtime = r match {
    case t: Thunk => t.state match {
      case Delayed(env, c) => whnf(eval(env, c), t :: chain)
      case BlackHole       => writeback(Bottom(sys.error("infinite loop detected")), chain)
      case Evaluated(r)    => writeback(r, chain)
    }
    case _ => writeback(r, chain)
  }

  @annotation.tailrec
  private def writeback(answer: Runtime, chain: List[Thunk]): Runtime = chain match {
    case t :: ts =>
      t.state = Evaluated(answer)
      writeback(answer, ts)
    case Nil => answer
  }

  def panic(msg: String) = sys.error(s"PANIC: $msg")
}
