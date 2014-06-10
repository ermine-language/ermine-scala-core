package ermine
package eval

import syntax.G._

object GEval {

  class Address(var value: Closure)

  sealed abstract class Closure
    case object BlackHole extends Closure
    case class PartialApplication(code: LambdaForm, env: Env, arity: Sorted[Int]) extends Closure
    case class PlainClosure(code: LambdaForm, env: Env) extends Closure
    // case class PrimClosure(wat?) extends Closure

  case class Env(envB: Vector[Address], envU: Vector[Long], envN: Vector[Any])

  sealed abstract class Frame
    case class Branch(branches: Continuation, env: Env) extends Frame
    case class Update(addr: Address) extends Frame

  case class MachineState(
    stackP: Sorted[Int],
    frameP: Sorted[Int],
    stackF: List[(Sorted[Int], Frame)],
    // genv: Map[Id, Address],
    trace: String => Unit,
    stackB: Array[Address],
    stackU: Array[Long],
    stackN: Array[Any]
  ) {
    def subSorted(a: Sorted[Int], b: Sorted[Int]): Sorted[Int] =
      Sorted(a.b - b.b, a.u - b.u, a.n - b.n)
    def addSorted(a: Sorted[Int], b: Sorted[Int]): Sorted[Int] =
      Sorted(a.b + b.b, a.u + b.u, a.n + b.n)

    def pop(args: Sorted[Int] = Sorted(0,0,0)): Option[(Frame, MachineState)] =
      stackF match {
        case (ofp, fr) :: fs =>
          Some((fr, squash(subSorted(frameP, stackP), args)
            .copy(frameP = ofp, stackF = fs)))
          case Nil => None
      }
    def squash(size: Sorted[Int], args: Sorted[Int]): MachineState = {
      def copyArgs[A](stk: Array[A], sp: Int, sz: Int, ar: Int) =
        Array.copy(stk, sp, stk, sp + sz - ar, ar)

      copyArgs(stackB, stackP.b, size.b, args.b)
      copyArgs(stackU, stackP.u, size.u, args.u)
      copyArgs(stackN, stackP.n, size.n, args.n)

      0 to (size.b - args.b) foreach { i => stackB(i) = null }
      0 to (size.n - args.n) foreach { i => stackN(i) = null }

      copy(stackP = addSorted(stackP, subSorted(size, args)))
    }
  }

  sealed abstract class State
    case class Eval(code: G, localEnv: Env) extends State
    case class Enter(addr: Address) extends State
    case class ReturnCon(tag: Long, args: Sorted[Int]) extends State
    case class ReturnLit(value: Long) extends State

  // @annotation.tailrec
  def execute(state: State, ms: MachineState): Unit = state match {
    case Eval(code, localEnv) => ()
    case Enter(addr) => ()
    case ReturnCon(tag, args) => ()
    case ReturnLit(value) => ()
  }
}
