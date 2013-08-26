package ermine.eval

import Eval.panic

object PrimOps {
  def primOpDefs: Map[String, Runtime] = Map (
    "eqString"     -> primOp((s1 : String, s2 : String) => s1 == s2),
    "stringAppend" -> primOp((s1 : String, s2 : String) => s1 + s2),

    "eqChar"       -> primOp((s1 : Char, s2 : Char) => s1 == s2),

    "eqInt"        -> primOp((i1 : Int, i2 : Int) => i1 == i2),
    "minusInt"     -> primOp((i1 : Int, i2 : Int) => i1 - i2),
    "plusInt"      -> primOp((i1 : Int, i2 : Int) => i1 + i2),
    "multInt"      -> primOp((i1 : Int, i2 : Int) => i1 * i2),
    "divInt"       -> primOp((i1 : Int, i2 : Int) => i1 / i2),
    "modInt"       -> primOp((i1 : Int, i2 : Int) => i1 % i2)
  )

  def adjustBooleans(a: Any) = a match {
    case false => Data(0, Nil)
    case true  => Data(1, Nil)
    case _     => Prim(a)
  }

  def primOp[T](f: Function1[T, Any]): Runtime =
    Func(1, { case List(arg1) => Eval.whnf(arg1) match {
      case Prim(x : T @unchecked) => adjustBooleans(f(x))
      case _ => panic("prim-op called with wrong number of arguments")
    }})

  def primOp[T, U](f: Function2[T, U, Any]): Runtime =
    Func(2, { case List(arg1, arg2) => (Eval.whnf(arg1), Eval.whnf(arg2)) match {
      case (Prim(x : T @unchecked), Prim(y : U @unchecked)) => adjustBooleans(f(x,y))
      case _ => panic("prim-op called with wrong number of arguments")
    }})

  def primOp[T,U,V](f: Function3[T, U, V, Any]): Runtime =
    Func(3, { case List(arg1, arg2, arg3) => (Eval.whnf(arg1), Eval.whnf(arg2), Eval.whnf(arg3)) match {
      case (Prim(x : T @unchecked), Prim(y : U @unchecked), Prim(z : V @unchecked)) => adjustBooleans(f(x,y,z))
      case _ => panic("prim-op called with wrong number of arguments")
    }})
}
