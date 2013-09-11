package ermine
package syntax

import scalaz.{Equal, Scalaz}
import scalaz.std.anyVal._
import scalaz.std.string._
import scalaz.scalacheck.ScalazProperties.{equal, monad, order, traverse}
import CoreArbitraryInstances._
import org.scalacheck.Prop
import org.scalacheck.Prop._
import org.scalacheck.Properties

object CoreTests extends ErmineProperties("Core Tests"){
  test("Assoc order")(order.laws[Assoc])
  test("Fixity order")(order.laws[Fixity])
  test("Digest order")(order.laws[Digest])
  test("ModuleName order")(order.laws[ModuleName])
  test("Global order")(order.laws[Global])
  test("Module order")(equal.laws[Module[Int]])
  test("Core Int equal")(equal.laws[Core[Int]])
  test("Core String equal")(equal.laws[Core[String]])
  test("HardCore equal")(equal.laws[HardCore])
  test("Core monad")(monad.laws[Core])
  //test("Core traverse")(traverse.laws[Core])
}
