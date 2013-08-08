package ermine.syntax

import org.scalacheck.{Prop, Arbitrary}
import org.scalacheck.Prop._
import f0._
import f0.Readers._
import f0.Writers._
import f0.DynamicF
import scalaz.{Equal, Scalaz}
import Scalaz._
import CoreArbitraryInstances._
import CoreSerialization._

object CoreSerializationTests extends ErmineProperties("CoreSerializationTests") {

  test("core == put/get core")(clone(coreW(intW), coreR(intR)))

  def clone[A,F](w: Writer[A,F], r: Reader[A,F])(implicit eql: Equal[A], arb: Arbitrary[A]): Prop =
    forAll((a: A) => { r(w.toByteArray(a)) === a })
}

object RoundTripTest extends ErmineProperties("RoundTripTest") {

  test("roundtrip")(roundtrip(hardcoreW, hardcoreR))

  lazy val coreEchoExists = new java.io.File("../ermine/dist/build/core-echo/core-echo.exe").exists

  def roundtrip[A,F](w: Writer[A,F], r: Reader[A,F])
                    (implicit eql: Equal[A], arb: Arbitrary[A]): Prop = forAll{(a: A) =>
    def callHaskellEcho(aOut: A) = {
      Sinks.toFile("../ermine/core.in").using{ s => w.bind(s)(aOut) }
      val p = Runtime.getRuntime.exec("../ermine/dist/build/core-echo/core-echo.exe")
      p.waitFor
      Sources.fromFile("../ermine/core.in")(r)
    }
    coreEchoExists ==> (callHaskellEcho(a) === a)
  }

//  def roundtrip[A,F](w: Writer[A,F], r: Reader[A,F])(implicit eql: Equal[A], arb: Arbitrary[A]): Prop = Prop(prms => {
//    def callHaskellEcho(aOut: A) = {
//      println(aOut)
//      Sinks.toFile("../ermine/core.in").using{ s => w.bind(s)(aOut) }
//      val p = Runtime.getRuntime.exec("../ermine/dist/build/core-echo/core-echo.exe")
//      p.waitFor
//      Sources.fromFile("../ermine/core.in")(r)
//    }
//    val aOut: A = arb.arbitrary(prms.genPrms).get
//    Prop.all(callHaskellEcho(aOut) === aOut)(prms)
//  })
}

//      Sinks.toProcess(p).using{s =>
//        w.bind(s)(aOut)
//        println("done writing!")
//        val res = Sources.fromProcess(p)(r)
//        println("done reading!")
//        res
//      }
