package free.control 

import cats.data.Coproduct
import cats.free.{Free, Inject}
import cats.syntax.either._
import cats.{Monad, ~>}
import free.control.ControlFlowA._

sealed trait ControlFlowA[A]

object ControlFlowA {
  private[control] final case object Stay extends ControlFlowA[Unit]
  private[control] final case class Const[A](a: A) extends ControlFlowA[A]

  private[control] final case object PushScope extends ControlFlowA[Unit]
  private[control] final case object PopScope extends ControlFlowA[Unit]

  private[control] final case class Label(lbl: String) extends ControlFlowA[Unit]
  private[control] final case class Goto(lbl: String) extends ControlFlowA[Unit]

  private[control] final case class Cond[F[_], A](conds: List[(Free[F, Boolean], Free[F, A])], otherwise: Free[F, A]) extends ControlFlowA[A]
  private[control] final case class While[F[_], A](cond: Free[F, Boolean], fn: Free[F, A]) extends ControlFlowA[Unit]
  private[control] final case class RepeatWithIndex[F[_], A](n: Free[F, Int], fn: Int => Free[F, A]) extends ControlFlowA[Unit]
}

class ControlFlow[F[_]](implicit I: Inject[ControlFlowA, F]) {

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

object ControlFlow {
  implicit def controlFlow[F[_]](implicit I: Inject[ControlFlowA, F]): ControlFlow[F] = new ControlFlow[F]

  type ControlFlowApp[F[_], A] = Coproduct[F, ControlFlowA, A]
  type FreeControlFlowApp[F[_], A] = Free[ControlFlowApp[F, ?], A]

  def interpreter[F[_], G[_]: Monad, I <: F ~> G](initial: I, copy: I => I): ControlFlowApp[F, ?] ~> G =
    new Interpreter[F, G, I](initial, copy)

  private[control] class Interpreter[F[_], G[_], I <: F ~> G](initial: I, copy: I => I)(implicit M: Monad[G]) extends (ControlFlowApp[F, ?] ~> G) {
    var stack: List[I] = List(initial)
    var labels: Map[String, FreeControlFlowApp[F, _]] = Map.empty

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
          if (c == nn - 1) Free.liftF[ControlFlowApp[F, ?], Unit](Coproduct.rightc(Stay))
          else fn(c).flatMap(_ => inner(c + 1))
        }
        inner(0)
      }
    }

    private def unsafe[AA](fa: Free[Any, _]): FreeControlFlowApp[F, AA] = fa.asInstanceOf[FreeControlFlowApp[F, AA]]
  }
}