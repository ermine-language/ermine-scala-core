package ermine.syntax

import bound._
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

  lazy val coreEchoExists = new java.io.File("../ermine/dist/build/core-echo/core-echo.exe").exists

  test("roundtrip")(forAll{(a: Core[Int]) => coreEchoExists ==> (callHaskellEcho(coreW(intW), coreR(intR))(a) === a)})

  def callHaskellEcho[A, F](w: Writer[A,F], r: Reader[A,F])(aOut: A)(implicit eql: Equal[A], arb: Arbitrary[A]): A = {
    import java.io._
    println(s"\n$aOut\n")
    new java.io.File("core.in.toString").delete
    new PrintWriter("core.in.toString").println(aOut)
    new File("core.in").delete
    new File("core.out").delete
    Sinks.toFile("core.in").using{ s => w.bind(s)(aOut) }
    val p = Runtime.getRuntime.exec("../ermine/dist/build/core-echo/core-echo.exe")
    p.waitFor
    Sources.fromFile("core.out")(r)
  }
}

//  val x: Core[Int] = Case(
//    Data(1, List()),
//    Map(1.toByte -> (0.toByte -> Scope(Let(List(),Scope(Slot(1)))))),
//    Some(Scope(LitString("")))
//  )
//  test("x")(x === callHaskellEcho(coreW(intW), coreR(intR))(x))

//test("roundtrip A")(coreEchoExists ==> (callHaskellEcho(hardcoreW, hardcoreR)(LitChar('A')) === LitChar('A')))
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

//      Sinks.toProcess(p).using{s =>
//        w.bind(s)(aOut)
//        println("done writing!")
//        val res = Sources.fromProcess(p)(r)
//        println("done reading!")
//        res
//      }