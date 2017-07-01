package free.control 

import cats.data.Coproduct
import cats.free.Free
import cats.{Id, ~>}

object App {

  type FunApp[A] = Coproduct[CounterA, ControlFlowA, A]

  def program(implicit C: Counter[FunApp], F: ControlFlow[FunApp]): Free[FunApp, (Int, Int, Int, Int, Int, Int)] = {
    import C._, F._

    for {
      mult <- function1F[Int, Unit] { x =>
        for {
          a <- get
          _ <- condF(
            constF(x == 0) -> reset,
            constF(x == 1) -> stayF)(
            otherwise = repeatF(constF(x))(add(a)))
        } yield ()
      }
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
      _ <- labelF("label")
      _ <- add(-1)
      _ <- condF(get.map(_ > 10) -> gotoF("label"))(otherwise = stayF)
      last <- get
    } yield (x, y, z, s, ss, last) // should be (15, 0, 128, 42, 128, 10)
  }

  def interpreter: FunApp ~> Id = ControlFlow.interpreter(Counter.interpreter, Counter.Interpreter.copy)

  def run = program.foldMap(interpreter)
}
