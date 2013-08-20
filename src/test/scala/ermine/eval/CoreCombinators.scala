package ermine.eval

import scalaz._
import Scalaz._
import ermine.syntax._
import ermine.syntax.{ Data => CoreData }
import bound._
import Core._

trait CoreCombinators {

  implicit class MkVar(val sc: StringContext) {
    def v(args: Any*): Core[String] = Var(sc.parts.mkString)
  }

  def indexWhere[A](a: A, as: Seq[A])(implicit e: Equal[A]): Option[Byte] = {
    val index = as.indexWhere(_ === a).toByte
    if(index == -1) None else Some(index)
  }

  // combinator for building dictionaries
  def dict(slots: (String, Core[String])*): Dict[String] = Dict(Nil, {
    val (x, y) = slots.unzip
    y.map(c => abstrakt(c)(indexWhere(_, x))).toList
  })

  // combinator for building case statements
  def cases(c: Core[String], branches: (Int, (List[String], Core[String]))*): Case[String] = Case(
    c, branches.toMap.mapKeys(_.toByte).mapValues{
      case (vars, cr) => ((0:Byte), abstrakt(cr)(indexWhere(_, vars).map((x:Byte) => (x + 1).toByte)))
    }, None
  )

  //  A smart constructor for Lamb
  def lam[A,F[+_]](vs: A*)(body: Core[A])(implicit m: Monad[F], e: Equal[A]): Lam[A] =
    Lam(vs.size.toByte, abstrakt(body)(b => indexWhere(b, vs.toList)))

  def let_[A](es: List[(A, Core[A])], e:Core[A]): Core[A] = es match {
    case Nil => e
    case _ =>
      def abstr(e:Core[A]) = abstractR((a:A) => {
        val i = es.map(_._1).indexOf(a)
        if(i>=0) Some(i.toByte) else None
      })(e)
      Let(es.map(t => abstr(t._2)), abstr(e))
  }

  def abstractR[B,F[+_],A](f : A => Option[B])(w : F[A])(implicit M: scalaz.Monad[F]) = abstrakt(w)(f)

  implicit class PimpedCore(e: Core[String]) {
    def !:(s:String) = lam[String, Core](s)(e)
  }

  implicit def intToLitInt(i:Int) = LitInt(i)

  def closed[A, B](fa:Core[A]): Option[Core[B]] = {
    implicit val x = Core.coreTraversable
    fa.traverse(Function.const(None))
  }
}
