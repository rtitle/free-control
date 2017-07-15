package free.control 

import cats.free.{Free, Inject}
import cats.syntax.either._
import free.control.ControlFlowA._

sealed trait ControlFlowA[A]

object ControlFlowA {
  private[control] final case object Stay extends ControlFlowA[Unit]
  private[control] final case class Const[A](a: A) extends ControlFlowA[A]

  private[control] final case object PushScope extends ControlFlowA[Unit]
  private[control] final case object PopScope extends ControlFlowA[Unit]
  private[control] final case class GetVar[F[_], A](name: String) extends ControlFlowA[Option[A]]
  private[control] final case class SetVar[F[_], A](name: String, value: Free[F, A]) extends ControlFlowA[Unit]

  private[control] final case class Label(lbl: String) extends ControlFlowA[Unit]
  private[control] final case class Goto(lbl: String) extends ControlFlowA[Unit]

  private[control] final case class Cond[F[_], A](conds: List[(Free[F, Boolean], Free[F, A])], otherwise: Free[F, A]) extends ControlFlowA[A]
  private[control] final case class While[F[_], A](cond: Free[F, Boolean], fn: Free[F, A]) extends ControlFlowA[Unit]
  private[control] final case class RepeatWithIndex[F[_], A](n: Free[F, Int], fn: Int => Free[F, A]) extends ControlFlowA[Unit]
}

private[control] class ControlFlow[F[_]](implicit I: Inject[ControlFlowA, F]) {

  def function0F[A](body: () => Free[F, A]): Free[F, () => Free[F, A]] =
    constF(body)

  def function1F[A, B](body: A => Free[F, B]): Free[F, A => Free[F, B]] =
    constF(body)

  def function2F[A, B, C](body: (A, B) => Free[F, C]): Free[F, (A, B) => Free[F, C]] =
    constF(body)

  def function3F[A, B, C, D](body: (A, B, C) => Free[F, D]): Free[F, (A, B, C) => Free[F, D]] =
    constF(body)

  def pushScopeF: Free[F, Unit] = Free.inject[ControlFlowA, F](PushScope)

  def popScopeF: Free[F, Unit] = Free.inject[ControlFlowA, F](PopScope)

  def getVarF[A](name: String): Free[F, Option[A]] = Free.inject[ControlFlowA, F](GetVar(name))

  def setVarF[A](name: String, value: Free[F, A]): Free[F, Unit] = Free.inject[ControlFlowA, F](SetVar(name, value))

  def stayF: Free[F, Unit] = Free.inject[ControlFlowA, F](Stay)

  def constF[A](a: A): Free[F, A] = Free.inject[ControlFlowA, F](Const(a))

  def labelF(lbl: String): Free[F, Unit] = Free.inject[ControlFlowA, F](Label(lbl))

  def gotoF(lbl: String): Free[F, Unit] = Free.inject[ControlFlowA, F](Goto(lbl))

  def condF[A](conds: (Free[F, Boolean], Free[F, A])*)(otherwise: Free[F, A]): Free[F, A] =
    Free.inject[ControlFlowA, F](Cond(conds.toList, otherwise))

  def partialCondF[A](condsF: (Free[F, Boolean], Free[F, A])*): Free[F, Either[Unit, A]] = {
    val rights = condsF.map { case (c, fn) => c -> fn.map(_.asRight[Unit])}
    val left = stayF.map(_.asLeft[A])
    condF(rights:_*)(left)
  }

  def whileF[A](cond: => Free[F, Boolean])(fn: Free[F, A]): Free[F, Unit] =
    Free.inject[ControlFlowA, F](While(cond, fn))

  def repeatWithIndexF[A](nF: Free[F, Int])(fn: Int => Free[F, A]): Free[F, Unit] =
    Free.inject[ControlFlowA, F](RepeatWithIndex(nF, fn))

  def repeatF(n: Free[F, Int])(fn: Free[F, Unit]): Free[F, Unit] =
    repeatWithIndexF(n)(_ => fn)

}