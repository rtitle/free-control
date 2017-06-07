package free.control 

import cats.data.Coproduct
import cats.free.{Free, Inject}
import cats.syntax.either._
import cats.{Monad, ~>}
import free.control.ControlFlowA._

sealed trait ControlFlowA[A]

object ControlFlowA {
  final case class Def[F[_], A, B](fn: A => Free[F, B]) extends ControlFlowA[A => Free[F, B]]
  final case object Stay extends ControlFlowA[Unit]
  final case object PushScope extends ControlFlowA[Unit]
  final case object PopScope extends ControlFlowA[Unit]
}

class ControlFlow[F[_]](implicit I: Inject[ControlFlowA, F]) {
  def defF[A, B](body: A => Free[F, B]): Free[F, A => Free[F, B]] =
    Free.inject[ControlFlowA, F](Def[F, A, B](body))

  def pushScope: Free[F, Unit] = Free.inject[ControlFlowA, F](PushScope)

  def popScope: Free[F, Unit] = Free.inject[ControlFlowA, F](PopScope)

  def stayF: Free[F, Unit] = Free.inject[ControlFlowA, F](Stay)

  def repeatF[A](n: Int)(fn: Free[F, A]): Free[F, Unit] = {
    def inner(c: Int): Free[F, Unit] = {
      if (c <= 0) stayF
      else fn.flatMap(_ => inner(c - 1))
    }
    inner(n - 1)
  }

  def whileF[A](cond: => Free[F, Boolean])(fn: Free[F, A]): Free[F, Unit] = {
    def inner: Free[F, Unit] = {
      cond.flatMap {
        case true => fn.flatMap(_ => inner)
        case false => stayF
      }
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

  def scopedInterpreter[F[_], G[_]: Monad](wrapped: => ControlFlowApp[F, ?] ~> G): ControlFlowApp[F, ?] ~> G =
    new ScopedInterpreter[F, G](wrapped)

  private class Interpreter[G[_]](implicit M: Monad[G]) extends (ControlFlowA ~> G) {
    def apply[A](fa: ControlFlowA[A]): G[A] = fa match {
      case Def(fn) =>
        M.pure(fn)
      case Stay =>
        M.pure(())
      case PushScope =>
        M.pure(())
      case PopScope =>
        M.pure(())
    }
  }

  private class ScopedInterpreter[F[_], G[_]](wrapped: => ControlFlowApp[F, ?] ~> G)(implicit M: Monad[G]) extends (ControlFlowApp[F, ?] ~> G) {
    var stack: List[ControlFlowApp[F, ?] ~> G] = List(wrapped)

    override def apply[A](fa: ControlFlowApp[F, A]): G[A] = {
      fa.run match {
        case Right(PushScope) =>
          val newInterpreter = wrapped
          stack = newInterpreter :: stack
          newInterpreter.apply(fa)
        case Right(PopScope) =>
          stack = stack.drop(1)
          stack.head.apply(fa)
        case _ => stack.head.apply(fa)
      }
    }
  }
}
