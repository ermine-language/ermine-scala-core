package ermine
package syntax

import scalaz.{Equal, Scalaz}
import Scalaz._
import CoreArbitraryInstances._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties

object CoreTests extends ErmineProperties("Core Tests"){

  test("== reflexivity for Core (with Int)")(forAll{ (core:Core[Int]) => eqls(core, core) })
  test("== reflexivity for Core (with String)")(forAll{ (core:Core[String]) => eqls(core, core) })
  test("== reflexivity for HardCore")(forAll{ (hc:HardCore) => HardCore.hardcoreEqual.equal(hc, hc) })
  test("== reflexivity for Module")(forAll{ (m:Module[Int]) => eqls(m, m) })

  def eqls[A](a: A, b: A)(implicit eql: Equal[A]) = eql.equal(a, b)
}

