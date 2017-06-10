package free.control 

import cats.data.Coproduct
import cats.free.{Free, Inject}
import cats.syntax.either._
import cats.{Monad, ~>}
import free.control.ControlFlow.Interpreter
import free.control.ControlFlowA._

sealed trait ControlFlowA[A]

object ControlFlowA {
  private[control] final case class Function0[F[_], A](fn: () => Free[F, A]) extends ControlFlowA[() => Free[F, A]]
  private[control] final case class Function1[F[_], A, B](fn: A => Free[F, B]) extends ControlFlowA[A => Free[F, B]]
  private[control] final case class Function2[F[_], A, B, C](fn: (A, B) => Free[F, C]) extends ControlFlowA[(A, B) => Free[F, C]]
  private[control] final case class Function3[F[_], A, B, C, D](fn: (A, B, C) => Free[F, D]) extends ControlFlowA[(A, B, C) => Free[F, D]]
  // that's enough for now

  private[control] final case object Stay extends ControlFlowA[Unit]

  private[control] final case object PushScope extends ControlFlowA[Unit]
  private[control] final case object PopScope extends ControlFlowA[Unit]

  private[control] final case class Label(lbl: String) extends ControlFlowA[Unit]
  private[control] final case class Goto(lbl: String) extends ControlFlowA[Unit]
}

class ControlFlow[F[_]](implicit I: Inject[ControlFlowA, F]) {

  def function0F[A](body: () => Free[F, A]): Free[F, () => Free[F, A]] =
    Free.inject[ControlFlowA, F](Function0(body))

  def function1F[A, B](body: A => Free[F, B]): Free[F, A => Free[F, B]] =
    Free.inject[ControlFlowA, F](Function1(body))

  def function2F[A, B, C](body: (A, B) => Free[F, C]): Free[F, (A, B) => Free[F, C]] =
    Free.inject[ControlFlowA, F](Function2(body))

  def function3F[A, B, C, D](body: (A, B, C) => Free[F, D]): Free[F, (A, B, C) => Free[F, D]] =
    Free.inject[ControlFlowA, F](Function3(body))

  // TODO: implement fixF0,1,2,3 for recursion

  def pushScopeF: Free[F, Unit] = Free.inject[ControlFlowA, F](PushScope)

  def popScopeF: Free[F, Unit] = Free.inject[ControlFlowA, F](PopScope)

  def stayF: Free[F, Unit] = Free.inject[ControlFlowA, F](Stay)

  def labelF(lbl: String): Free[F, Unit] = Free.inject[ControlFlowA, F](Label(lbl))

  def gotoF(lbl: String): Free[F, Unit] = Free.inject[ControlFlowA, F](Goto(lbl))

  def repeatF[A](n: Int)(fn: Free[F, A]): Free[F, Unit] =
    repeatWithIndexF(n)(_ => fn)

  def repeatWithIndexF[A](n: Int)(fn: Int => Free[F, A]): Free[F, Unit] = {
    def inner(c: Int): Free[F, Unit] = {
      if (c == n - 1) stayF
      else fn(c).flatMap(_ => inner(c + 1))
    }
    inner(0)
  }

  def whileF[A](cond: => Free[F, Boolean])(fn: Free[F, A]): Free[F, Unit] = {
    def inner: Free[F, Unit] = cond.flatMap {
      case true => fn.flatMap(_ => inner)
      case false => stayF
    }
    inner
  }

  def condF[A](conds: (Boolean, Free[F, A])*): Free[F, Either[Unit, A]] = {
    val rights = conds.map { case (c, fn) => c -> fn.map(_.asRight[Unit])}
    val left = stayF.map(_.asLeft[A])
    condF_(rights:_*)(left)
  }

  def condF_[A](conds: (Boolean, Free[F, A])*)(otherwise: Free[F, A]): Free[F, A] = {
    def inner(cs: List[(Boolean, Free[F, A])]): Free[F, A] = cs match {
      case Nil => otherwise
      case (true, fn) :: _ => fn
      case _ :: t => inner(t)
    }
    inner(conds.toList)
  }
}

object ControlFlow {
  implicit def controlFlow[F[_]](implicit I: Inject[ControlFlowA, F]): ControlFlow[F] = new ControlFlow[F]

  type ControlFlowApp[F[_], A] = Coproduct[F, ControlFlowA, A]

  def interpreter[G[_]: Monad]: ControlFlowA ~> G = new Interpreter[G]

  def scopedInterpreter[F[_], G[_]: Monad, I <: F ~> G](initial: I, copy: I => I): ControlFlowApp[F, ?] ~> G =
    new ScopedInterpreter[F, G, I](initial, copy)

  def gotoInterpreter[F[_], G[_]: Monad](wrapped: ControlFlowApp[F, ?] ~> G): ControlFlowApp[F, ?] ~> G =
    new GotoInterpreter[F, G](wrapped)

  private[control] class Interpreter[G[_]](implicit M: Monad[G]) extends (ControlFlowA ~> G) {

    def apply[A](fa: ControlFlowA[A]): G[A] = fa match {
      case Function0(fn) =>
        M.pure(fn)

      case Function1(fn) =>
        M.pure(fn)

      case Function2(fn) =>
        M.pure(fn)

      case Function3(fn) =>
        M.pure(fn)

      case Stay =>
        M.pure(())

      case PushScope =>
        M.pure(())

      case PopScope =>
        M.pure(())

      case Label(_) =>
        M.pure(())

      case Goto(_) =>
        M.pure(())
    }
  }

  private class ScopedInterpreter[F[_], G[_], I <: F ~> G](initial: I, copy: I => I)(implicit M: Monad[G]) extends (ControlFlowApp[F, ?] ~> G) {
    var stack: List[I] = List(initial)

    override def apply[A](fa: ControlFlowApp[F, A]): G[A] = {
      fa.run match {
        case Right(PushScope) =>
          stack = stack match {
            case h :: t => copy(h) :: h :: t
            case _ => stack
          }
          M.pure(())
        case Right(PopScope) =>
          stack = stack match {
            case _ :: t => t
            case _ => stack
          }
          M.pure(())
        case _ => (stack.head or interpreter[G]).apply(fa)
      }
    }
  }

  private class GotoInterpreter[F[_], G[_]](wrapped: ControlFlowApp[F, ?] ~> G)(implicit M: Monad[G]) extends (ControlFlowApp[F, ?] ~> G) {
    var program: Free[ControlFlowApp[F, ?], Any] = Free.pure[ControlFlowApp[F, ?], Any](Coproduct.rightc(Stay))
    var labels: Map[String, Free[ControlFlowApp[F, ?], _]] = Map.empty

    override def apply[A](fa: ControlFlowApp[F, A]): G[A] = {
      fa.run match {
        case Right(rfa) =>
          val step = Free.liftF[ControlFlowApp[F, ?], A](Coproduct.rightc(rfa))
          // If Free were covariant on type A I wouldn't have to cast to Any
          program = program.flatMap(_ => step.asInstanceOf[Free[ControlFlowApp[F, ?], Any]])

          rfa match {
            case Label(lbl) =>
              // TODO bug here: the step doesn't reference the entire program so goto doesn't work
              labels = labels + (lbl -> step)
            case Goto(lbl) =>
              labels.get(lbl).foreach(_.foldMap(this))
            case _ =>
          }

        case Left(lfa) =>
          val step = Free.liftF[ControlFlowApp[F, ?], A](Coproduct.leftc(lfa))
          // If Free were covariant on type A I wouldn't have to cast to Any
          program = program.flatMap(_ => step.asInstanceOf[Free[ControlFlowApp[F, ?], Any]])
      }
      wrapped.apply(fa)
    }
  }
}