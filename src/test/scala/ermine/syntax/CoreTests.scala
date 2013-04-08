package ermine.syntax


import scalaz.{Equal, Scalaz}
import Scalaz._
import CoreArbitraryInstances._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties

object CoreTests extends ErmineProperties("Core Tests"){

  test("== reflexivity for Core (with Int)")(forAll{ (core:Core[Int]) => eqls(core, core) })
  test("== reflexivity for Core (with String)")(forAll{ (core:Core[String]) => eqls(core, core) })

  def eqls[A](a: Core[A], b: Core[A])(implicit eql: Equal[Core[A]]) = {
    eql.equal(a, b)
  }
}

abstract class ErmineProperties(name: String) extends Properties(name){
  def test(name:String)(f: => Prop) = property(name) = secure {
    try f catch { case e: java.lang.Throwable  => e.printStackTrace(System.err); throw e }
  }
}