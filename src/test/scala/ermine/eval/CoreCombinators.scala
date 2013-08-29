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

  implicit class MkGlobal(val sc: StringContext) {
    def g(args: Any*): Global = {
      val s = sc.parts.mkString
      val i = s.lastIndexOf(".")
      Global(ModuleName("ermine", s.substring(0, i)), s.substring(i+1))
    }
  }

  implicit class MkModule(val sc: StringContext) {
    def m(args: Any*): ModuleName = ModuleName("ermine", sc.parts.mkString)
  }

  implicit class ToModule(s:String) {
    def m = ModuleName("ermine", s)
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
  def cases(c: Core[String], branches: (Int, (List[String], Core[String]))*): Case[String] =
    gcases(c, "_", None, branches:_*)

  /** Fully general case statement builder. Case statements have the general form:
   *
   *   case e of x {
   *     _DEFAULT_ -> ...
   *     0 [vs] -> ...
   *     ...
   *
   * where x is bound to the whnf of e, and scopes over all branches, including the
   * default, and the _DEFAULT_ case is present iff the rest of the cases are not
   * exhaustive.
   */
  def gcases(c: Core[String],
             as: String,
             default: Option[Core[String]],
             branches: (Int, (List[String], Core[String]))*): Case[String] = {
    val bs : Map[Byte, (Byte, Scope[Byte, Core, String])] = branches.map({
        case (tag, (vs, body)) => (tag.toByte, (vs.length.toByte, abstrakt(body)(indexWhere(_, as::vs))))
      }).toMap
    Case(c, bs, default.map(abstract1(as, _)))
  }

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
    def !:(ss:(String,String)) = ss match {case (s1, s2) => lam[String, Core](s1,s2)(e)}
    def !:(ss:(String,String,String)) = ss match {case (s1,s2,s3) => lam[String, Core](s1,s2,s3)(e)}
    def !:(ss:(String,String,String,String)) = ss match {case (s1,s2,s3,s4) => lam[String, Core](s1,s2,s3,s4)(e)}
    def !:(ss:(String,String,String,String,String)) = ss match {case (s1,s2,s3,s4,s5) => lam[String, Core](s1,s2,s3,s4,s5)(e)}
  }

  implicit def intToLitInt(i:Int) = LitInt(i)
  implicit def globalToRef(g:Global) = GlobalRef(g)

  def closed[A, B](fa:Core[A]): Option[Core[B]] = {
    implicit val x = Core.coreTraversable
    fa.traverse(Function.const(None))
  }
}
