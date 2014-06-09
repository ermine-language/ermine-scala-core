package ermine.syntax

object G {

  case class Sorted[A](b: A, u: A, n: A)

  sealed abstract class G extends Product with Serializable
  case class Case(data: G, branches: Continuation) extends G
  case class CaseLit(prim: Ref, branches: Continuation) extends G
  case class App(nrToPop: Sorted[Long], func: Func, args: Sorted[Vector[Ref]]) extends G
  case class Let(preclosures: Vector[PreClosure], body: G) extends G
  case class LetRec(preclosures: Vector[PreClosure], body: G) extends G
  case object Slot extends G

  sealed abstract class Ref
  // case class Global() // TODO: nail down what this should actually be
  case class Local(ix: Long) extends Ref
  case class Stack(ix: Long) extends Ref
  case class Lit(value: Long) extends Ref
  case class Native(value: Any) extends Ref

  type Tag = Long

  case class PreClosure(captures: Sorted[Vector[Ref]], code: LambdaForm)

  case class LambdaForm(
    free: Sorted[Long],
    bound: Sorted[Long],
    update: Boolean,
    body: G
  )

  case class Continuation(branches: Map[Tag, (Sorted[Long], G)], default: Option[G])

  sealed abstract class Func
  case class FuncRef(ref: Ref) extends Func
  case class Con(tag: Tag) extends Func
}
