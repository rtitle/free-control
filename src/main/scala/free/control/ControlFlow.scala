package free.control 

import cats.free.{Free, Inject}
import cats.syntax.either._
import cats.{Id, ~>}
import free.control.ControlFlow._
import free.control.ControlFlowA._

sealed trait ControlFlowA[A]

object ControlFlowA {
  final case class Def[F[_], A, B](fn: A => Free[F, B]) extends ControlFlowA[A => Free[F, B]]
  final case object Stay extends ControlFlowA[Unit]
}

class ControlFlow[F[_]](implicit I: Inject[ControlFlowA, F]) {
  def defF[A, B](body: A => Free[F, B]): Free[F, A => Free[F, B]] =
    Free.inject[ControlFlowA, F](Def[F, A, B](body))

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
}

object ControlFlow {
  implicit def controlFlow[F[_]](implicit I: Inject[ControlFlowA, F]): ControlFlow[F] = new ControlFlow[F]

  def condF_[F[_], A](conds: (Boolean, Free[F, A])*)(otherwise: Free[F, A]): Free[F, A] = {
    def inner(cs: List[(Boolean, Free[F, A])]): Free[F, A] = cs match {
      case Nil => otherwise
      case (true, fn) :: _ => fn
      case _ :: t => inner(t)
    }
    inner(conds.toList)
  }

  object Interpreter extends (ControlFlowA ~> Id) {
    var ifIsActive: Boolean = _

    def apply[A](fa: ControlFlowA[A]): Id[A] = fa match {
      case Def(fn) =>
        fn
      case Stay =>
        ()
    }
  }


}
