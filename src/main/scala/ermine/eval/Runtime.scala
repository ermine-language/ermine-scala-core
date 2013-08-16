package ermine.eval

import ermine.syntax.Core

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
case class Delayed[V](env: Map[V, Runtime], e: Core[V]) extends ThunkState

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
class Thunk(var state: ThunkState, val update: Boolean) extends Runtime

object Thunk {
  def apply[V](env: Map[V, Runtime], e: Core[V], update: Boolean) =
    new Thunk(Delayed(env, e), update)
}
