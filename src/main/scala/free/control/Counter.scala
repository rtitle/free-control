package free.control 

import cats.free.{Free, Inject}
import cats.{Monad, ~>}
import free.control.CounterA._

sealed trait CounterA[A]

object CounterA {
  final case class Set(n: Int) extends CounterA[Unit]
  final case class Add(n: Int) extends CounterA[Unit]
  final case class Subtract(n: Int) extends CounterA[Unit]
  final case object Get extends CounterA[Int]
  final case object Reset extends CounterA[Unit]
}

class Counter[F[_]](implicit I: Inject[CounterA, F]) {
  def set(n: Int): Free[F, Unit] = Free.inject[CounterA, F](Set(n))
  def add(n: Int): Free[F, Unit] = Free.inject[CounterA, F](Add(n))
  def subtract(n: Int): Free[F, Unit] = Free.inject[CounterA, F](Subtract(n))
  def get: Free[F, Int] = Free.inject[CounterA, F](Get)
  def reset: Free[F, Unit] = Free.inject[CounterA, F](Reset)
}

object Counter {
  implicit def counter[F[_]](implicit I: Inject[CounterA, F]): Counter[F] = new Counter[F]

  def interpreter[G[_]: Monad]: CounterA ~> G = new Interpreter[G]

  private class Interpreter[G[_]](implicit M: Monad[G]) extends (CounterA ~> G) {
    var curN: Int = _

    def apply[A](fa: CounterA[A]): G[A] = fa match {
      case Set(n) =>
        curN = n
        M.pure(())
      case Add(n) =>
        curN = curN + n
        M.pure(())
      case Subtract(n) =>
        curN = curN - n
        M.pure(())
      case Get =>
        M.pure(curN)
      case Reset =>
        curN = 0
        M.pure(())
    }
  }
}
