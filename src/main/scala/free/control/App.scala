package free.control 

import cats.data.Coproduct
import cats.free.Free
import cats.{Id, ~>}
import free.control.ControlFlow._

object App {

  type FunApp[A] = Coproduct[CounterA, ControlFlowA, A]

  def program(implicit C: Counter[FunApp], F: ControlFlow[FunApp]): Free[FunApp, (Int, Int, Int)] = {
    import C._, F._

    for {
      mult <- defF((x: Int) =>
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
    } yield (x, y, z) // should be (15, 0, 128)
  }

  val interpreter: FunApp ~> Id = Counter.Interpreter or ControlFlow.Interpreter

  def run = program.foldMap(interpreter)

}
