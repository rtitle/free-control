package control.free

import cats.{Id, ~>}
import cats.data.Coproduct
import org.scalatest.{FlatSpec, Matchers}

/**
  * Created by rtitle on 7/15/17.
  */
class ControlFlowSpec extends FlatSpec with Matchers {

  type FunApp[A] = Coproduct[CounterA, ControlFlowA, A]

  val C = implicitly[Counter[FunApp]]
  val F = implicitly[ControlFlow[FunApp]]

  import C._, F._

  def interpreter: FunApp ~> Id = ControlFlowInterpreter(Counter.interpreter, Counter.Interpreter.copy)

  "ControlFlow" should "support while loops" in {
    val program = for {
      _ <- whileF(get.map(_ < 100))(add(1))
      res <- get
    } yield res

    program.foldMap(interpreter) should equal (100)
  }

  it should "repeatWithIndex" in {
    val program = for {
      _ <- repeatWithIndexF(constF(10))(add)
      res <- get
    } yield res

    program.foldMap(interpreter) should equal (45)
  }

  it should "repeat" in {
    val program = for {
      _ <- repeatF(constF(10))(add(1))
      res <- get
    } yield res

    program.foldMap(interpreter) should equal (10)
  }

  it should "support conditionals" in {
    val program = for {
      _ <- set(5)
      res <- condF(get.map(_ < 10) -> constF(100))(otherwise = constF(-1))
    } yield res

    program.foldMap(interpreter) should equal (100)
  }

  it should "support partial conditionals" in {
    val program = for {
      _ <- set(5)
      res <- partialCondF(get.map(_ < 10) -> constF(100))
    } yield res

    program.foldMap(interpreter) should equal (Right(100))
  }

  it should "support functions" in {
    val program = for {
      mult <- function1F[Int, Unit] { x =>
        for {
          a <- get
          _ <- condF(
            constF(x == 0) -> reset,
            constF(x == 1) -> stayF)(
            otherwise = repeatF(constF(x - 1))(add(a)))
        } yield ()
      }
      _ <- set(5)
      _ <- mult(3)
      res <- get
    } yield res

    program.foldMap(interpreter) should equal (15)
  }

  it should "support scope" in {
    val program = for {
      _ <- set(1)
      _ <- pushScopeF
      _   <- set(42)
      a   <- get
      _ <- popScopeF
      b <- get
    } yield (a, b)

    program.foldMap(interpreter) should equal (42, 1)
  }

  it should "support goto" in {
    val program = for {
      _ <- set(100)
      _ <- labelF("lbl")
      _ <- add(-1)
      _ <- condF(get.map(_ > 10) -> gotoF("lbl"))(otherwise = stayF)
      res <- get
    } yield res

    program.foldMap(interpreter) should equal (10)
  }

  it should "support vars" in {
    val program = for {
      _ <- setVarF("v1", constF(3))
      _ <- setVarF("v2", constF(5))
      res1 <- getVarF[Int]("v1")
      res2 <- getVarF[Int]("v2")
      res3 <- getVarF[Int]("bogus")
    } yield (res1, res2, res3)

    program.foldMap(interpreter) should equal (Some(3), Some(5), None)
  }

}
