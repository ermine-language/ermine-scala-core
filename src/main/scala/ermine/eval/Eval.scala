package ermine.eval

import bound.Scope
import ermine.syntax.{Data => CoreData, _}
import Runtime.Env

object SessionEnv {
  def load(modules: Module[Int]*): SessionEnv = new SessionEnv().processModules(modules.toList)
}

case class SessionEnv(env: Env = Map(), globs: Map[Global, Address] = Map(), digests: Map[Digest, Address] = Map()){
  def processModule(mod: Module[Int]): SessionEnv = {
    val addrs = mod.definitions.map(_ => new Address)
    val amod  = mod.map(i => addrs(i))
    var newEnv : Env = null
    newEnv = this.env ++ amod.definitions.zip(addrs).map({ case (c, a) => a -> Thunk(newEnv, c, true) }).toMap
    SessionEnv(newEnv, this.globs ++ amod.termExports.map {
      case (g1, Left(g2))    => g1 -> this.globs(g2)
      case (g1, Right(addr)) => g1 -> addr
    }, this.digests ++ amod.instances)
  }
  def processModules(modules: List[Module[Int]]): SessionEnv = modules.foldLeft(this){ case (s, m) => s.processModule(m) }
}

object Eval {

  def die(msg: String) = throw Death(msg)

  var sessionEnv: SessionEnv = null

  @annotation.tailrec
  final def eval(env: Env, core: Core[Address], stk: List[Runtime] = Nil): Runtime = core match {
    case Var(a)            => appl(env.getOrElse(a, panic(s"bad variable reference $a")), stk)

    case Super(b)          => stk match {
      case Evidence(sups, _) :: rest => appl(sups(b.toInt), rest)
      case x :: _ => panic(s"Super applied to non-dictionary $x")
      case Nil => Func(1, { case List(Evidence(sups, _)) => sups(b.toInt) })
    }

    case Slot(b)          => stk match {
      case Evidence(_, slots) :: rest => appl(slots(b.toInt), rest)
      case (x :: _) => panic(s"Slot applied to non-dictionary $x")
      case Nil => Func(1, { case List(Evidence(_, slots)) => slots(b.toInt) })
    }

    case GlobalRef(g) => appl(env(sessionEnv.globs(g)), stk)

    case InstanceRef(d) => appl(env(sessionEnv.digests(d)), stk)

    case Err(msg)          => Bottom(die(msg))

    case l: Lit            => appl(Prim(l.extract), stk)

    case CoreData(tag, cs) => appl(Data(tag, cs.map(Thunk(env, _, true))), stk)

    case App(x, y)         => eval(env, x, Thunk(env, y, true) :: stk)

    case Lam(n, e)         =>
      // this case doesn't use java stack
      if(stk.length >= n) stk.splitAt(n.toInt) match {
        case (args, rest) =>
          val addrs = args.map(_ => new Address)
          val newEnv = addrs.zip(args).toMap ++ env
          eval(newEnv, e.instantiate(b => Var(addrs(b.toInt))), rest)
      }
      // TODO: this case does...
      else appl(Func(n, evalLam(env, e)(_.toInt)), stk)

    case Let(bs, e)     =>
      var newEnv : Env = null
      val addrs = bs.map(_ => new Address)
      val ibs = bs.map(b => b.instantiate(i => Var(addrs(i))))
      newEnv = env ++ addrs.zip(ibs).map { case (addr, b) => addr -> Thunk(newEnv, b, true) }
      eval(newEnv, e.instantiate(i => Var(addrs(i))), stk)

    case Case(e, bs, d) =>
      val (body, aug) = pickBranch(env, e, bs, d)
      eval(env ++ aug, body, stk)

    case Dict(xs, ys)   => appl(buildDict(env, xs, ys), stk)

    case LamDict(e)     => stk match {
      case Nil => Func(1, evalLam(env, e)(_ => 0))
      case dict :: rest =>
        val addr = new Address
        eval(env + (addr -> dict), e.instantiate(_ => Var(addr)), rest)
    }

    case AppDict(x, y)  =>  eval(env, x, evalDict(env, y) :: stk)

    case PrimOp(name) => appl(PrimOps.primOpDefs(name), stk)

    case f@ForeignMethod(static, _, _, _) => {
      def mk: Runtime =
        Func((f.arity + (if(static) 0 else 1)).toByte, args => {
          val evaledArgs = args.map(a => whnf(a) match {
            case Prim(e) => e.asInstanceOf[AnyRef]
            case _ => panic("Non-Prim in invocation of ForeignMethod.")
          })
          if (static)
            Prim(f.method.invoke(null, evaledArgs:_*))
          else
            Prim(f.method.invoke(evaledArgs.head, evaledArgs.tail:_*))
        })
      if (static && f.arity == 0) appl(Prim(f.method.invoke(null)), stk)
      else appl(mk, stk)
    }

    case f@ForeignConstructor(_, _) =>
      if(f.arity == 0) appl(Prim(f.con.newInstance()), stk)
      else appl(Func(f.arity, args => {
        val evaledArgs = args.map(a => whnf(a) match {
          case Prim(e) => e.asInstanceOf[AnyRef]
          case _ => panic("Non-Prim in invocation of ForiegnConstructor.")
        })
        Prim(f.con.newInstance(evaledArgs:_*))
      }), stk)

    case f@ForeignValue(static, _, _) =>
      if(static) appl(Prim(f.field.get(null)), stk)
      else appl(Func(1, args => args match {
        case List(x) => whnf(x) match {
          case Prim(e) => Prim(f.field.get(e.asInstanceOf[AnyRef]))
          case _ => panic("Non-Prim in invocation of ForiegnValue.")
        }
      }), stk)
  }

  def buildDict(env: Env, supers: List[Core[Address]], slots: List[Scope[Byte, Core, Address]]) = {
    val esupers = supers.map(eval(env, _))
    var newEnv : Env = null
    val slotAddrs = slots.map(_ => new Address)
    val eslots = slots.map(b => Thunk(newEnv, b.instantiate(i => Var(slotAddrs(i))), true))
    newEnv = env ++ slotAddrs.zip(eslots).toMap
    Evidence(esupers, eslots)
  }

  def evalDict(env: Env, y: Core[Address]) = whnf(eval(env, y))

  private def evalLam[T](env: Env, s:Scope[T, Core, Address])(f: T => Int): List[Runtime] => Runtime = l => {
    val addrs = l.map(_ => new Address)
    val newEnv = addrs.zip(l).toMap ++ env
    eval(newEnv, s.instantiate(b => Var(addrs(f(b)))))
  }

  private def pickBranch(env: Env, c: Core[Address],
                         branches: Map[Byte, (Byte, Scope[Byte, Core, Address])],
                         default: Option[Scope[Unit, Core, Address]]): (Core[Address], Env) = {
    lazy val evaledAddr = new Address
    def defaultCase(e: Runtime): (Core[Address], Env) = (default.get.instantiate(_ => Var(evaledAddr)), Map(evaledAddr -> e))

    whnf(eval(env, c)) match {
      case b: Bottom => (Var(evaledAddr), Map(evaledAddr -> b))
      case d@Data(tag, fields) =>
        branches.get(tag) match {
          case Some((_, body)) =>
            val addrs = fields.map(_ => new Address)
            (body.instantiate(b => Var((evaledAddr :: addrs)(b.toInt))), ((evaledAddr,d) :: addrs.zip(fields)).toMap)
          case None if default.isDefined => defaultCase(d)
          case _ => panic("non-exhaustive case statement without default")
        }
      case x if branches.size == 0 && default.isDefined => defaultCase(x)
      case x => panic(s"pick branch is confused: $x : ${x.getClass}")
    }
  }

  @annotation.tailrec
  private def appl(v: Runtime, stk: List[Runtime]): Runtime = stk match {
    case Nil => v
    case a :: stkp => whnf(v) match {
      case t : Thunk                   => sys.error("PANIC: whnf returned thunk")
      case b : Bottom                  => b
      case Func(arity, f)              =>
        if(stk.length < arity) PartialApp(v, stk)
        else {
          val (args, rest) = stk.splitAt(arity)
          appl(f(args), rest)
        }
      case PartialApp(f, args)         => appl(f, args ++ stk)
      case Prim(f : Function1[Any, _]) => ??? // appl(Prim(f(a.extract)), stkp)
      case f                           => panic(s"Cannot apply runtime value $f to $a")
    }
  }

  @annotation.tailrec
  final def whnf(r: Runtime, chain: List[Thunk] = Nil): Runtime = r match {
    case t: Thunk => t.state match {
      case Delayed(env, c) => whnf(eval(env, c), t :: chain)
      case BlackHole       => writeback(Bottom(sys.error("infinite loop detected")), chain)
      case Evaluated(r)    => writeback(r, chain)
    }
    case _ => writeback(r, chain)
  }

  @annotation.tailrec
  private def writeback(answer: Runtime, chain: List[Thunk]): Runtime = chain match {
    case t :: ts =>
      t.state = Evaluated(answer)
      writeback(answer, ts)
    case Nil => answer
  }

  def panic(msg: String) = sys.error(s"PANIC: $msg")
}
