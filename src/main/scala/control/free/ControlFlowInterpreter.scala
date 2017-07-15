package control.free

import cats.data.Coproduct
import cats.free.Free
import cats.{Monad, ~>}
import ControlFlowA._

/**
  * Created by rtitle on 7/15/17.
  */
object ControlFlowInterpreter {
  def apply[F[_], G[_]: Monad, I <: F ~> G](initial: I, copy: I => I): ControlFlowApp[F, ?] ~> G =
    new ControlFlowInterpreter[F, G, I](initial, copy)
}

class ControlFlowInterpreter[F[_], G[_], I <: F ~> G] private (initial: I, copy: I => I)(implicit M: Monad[G]) extends (ControlFlowApp[F, ?] ~> G) {
  var stack: List[I] = List(initial)
  var labels: Map[String, FreeControlFlowApp[F, _]] = Map.empty
  var vars: Map[String, Free[Any, Any]] = Map.empty

  override def apply[A](fa: ControlFlowApp[F, A]): G[A] = {
    fa.run match {
      case Left(lfa) =>
        val step = Free.liftF[ControlFlowApp[F, ?], A](Coproduct.leftc(lfa))
        labels = labels.mapValues(_.flatMap(_ => step))
        stack.head.apply(lfa)

      case Right(rfa) =>
        val step = Free.liftF[ControlFlowApp[F, ?], A](Coproduct.rightc(rfa))

        if (!rfa.isInstanceOf[Goto]) {
          labels = labels.mapValues(_.flatMap(_ => step))
        }

        rfa match {
          case Stay => M.pure(())

          case Const(a) => M.pure(a)

          case Cond(conds, otherwise) =>
            val unsafeConds = conds.map { case (a, b) => unsafe[Boolean](a) -> unsafe[A](b) }
            val unsafeOtherwise = unsafe[A](otherwise)
            condF(unsafeConds, unsafeOtherwise).foldMap(this)

          case While(cond, fn) =>
            whileF(unsafe[Boolean](cond), unsafe[Unit](fn)).foldMap(this)

          case RepeatWithIndex(n, fn) =>
            val unsafeFn = fn.asInstanceOf[Int => FreeControlFlowApp[F, A]]
            repeatWithIndexF(unsafe[Int](n), unsafeFn).foldMap(this)

          case PushScope =>
            stack = stack match {
              case h :: t => copy(h) :: h :: t
              case _ => stack
            }
            M.pure(())

          case PopScope =>
            stack = stack match {
              case _ :: t => t
              case _ => stack
            }
            M.pure(())

          case GetVar(name) =>
            val found = vars.get(name)
            M.pure(found.map(unsafe(_).foldMap(this)))

          case SetVar(name, value) =>
            vars = vars + (name -> value)
            M.pure(())

          case Label(lbl) if !labels.contains(lbl) =>
            labels = labels + (lbl -> step)
            M.pure(())

          case Goto(lbl) =>
            val routine = labels.get(lbl)
            labels = labels - lbl
            routine.foreach(_.foldMap(this))
            M.pure(())
        }
    }
  }

  private def condF[A](condsF: List[(FreeControlFlowApp[F, Boolean], FreeControlFlowApp[F, A])], otherwise: FreeControlFlowApp[F, A]): FreeControlFlowApp[F, A] = {
    def inner(cs: List[(FreeControlFlowApp[F, Boolean], FreeControlFlowApp[F, A])]): FreeControlFlowApp[F, A] = cs match {
      case Nil => otherwise
      case (cond, fn) :: t => cond.flatMap {
        case true => fn
        case false => inner(t)
      }
    }
    inner(condsF)
  }

  private def whileF[A](condF: FreeControlFlowApp[F, Boolean], fn: FreeControlFlowApp[F, A]): FreeControlFlowApp[F, Unit] = {
    def inner: FreeControlFlowApp[F, Unit] = condF.flatMap {
      case true => fn.flatMap(_ => inner)
      case false => Free.liftF[ControlFlowApp[F, ?], Unit](Coproduct.rightc(Stay))
    }
    inner
  }

  private def repeatWithIndexF[A](n: FreeControlFlowApp[F, Int], fn: Int => FreeControlFlowApp[F, A]): FreeControlFlowApp[F, Unit] = {
    n.flatMap { nn =>
      def inner(c: Int): FreeControlFlowApp[F, Unit] = {
        if (c == nn) Free.liftF[ControlFlowApp[F, ?], Unit](Coproduct.rightc(Stay))
        else fn(c).flatMap(_ => inner(c + 1))
      }
      inner(0)
    }
  }

  private def unsafe[AA](fa: Free[Any, _]): FreeControlFlowApp[F, AA] = fa.asInstanceOf[FreeControlFlowApp[F, AA]]
}
