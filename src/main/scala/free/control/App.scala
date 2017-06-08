package free.control 

import cats.data.Coproduct
import cats.free.Free
import cats.{Id, ~>}

object App {

  type FunApp[A] = Coproduct[CounterA, ControlFlowA, A]

  def program(implicit C: Counter[FunApp], F: ControlFlow[FunApp]): Free[FunApp, (Int, Int, Int, Int, Int)] = {
    import C._, F._

    for {
      mult <- function1F((x: Int) =>
        for {
          a <- get
          _ <- condF_(
            (x == 0) -> reset,
            (x == 1) -> stayF)(
            otherwise = repeatF(x)(add(a)))
        } yield ())
      _ <- set(5)
      _ <- mult(3)
      x <- get
      _ <- set(5)
      _ <- mult(0)
      y <- get
      _ <- set(1)
      _ <- whileF(get.map(_ < 100))(mult(2))
      z <- get
      _ <- pushScopeF
      _ <-   set(42)
      s <-   get
      _ <- popScopeF
      ss <- get
    } yield (x, y, z, s, ss) // should be (15, 0, 128, 42, 128)
  }

  def interpreter: FunApp ~> Id = Counter.interpreter or ControlFlow.interpreter[Id]
  val scopedInterpreter: FunApp ~> Id = ControlFlow.scopedInterpreter(interpreter)

  def run = program.foldMap(scopedInterpreter)
}
