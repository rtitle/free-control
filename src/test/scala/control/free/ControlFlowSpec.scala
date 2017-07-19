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
      _ <- whileF(get.map(_ < 100)) {
        add(1)
      }
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
      res <- condF(
        get.map(_ < 2) -> constF("less than 2"),
        get.map(_ < 5) -> constF("less than 5"),
        get.map(_ < 10) -> constF("less than 10"))(
        otherwise = constF("greater than or equal to 10"))
    } yield res

    program.foldMap(interpreter) should equal ("less than 10")
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
      _   <- add(42)
      a   <- get
      _ <- popScopeF
      b <- get
    } yield (a, b)

    program.foldMap(interpreter) should equal (43, 1)
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

  it should "run the Sieve of Eratosthenes" in {
    val sieve = (n: Int) => for {
      _ <- setVarF("primes", constF(Set.empty ++ (2 to n)))
      _ <- setVarF("p", constF(2))
      _ <- whileF(getVarF[Int]("p").map(p => p.get * p.get < n)) {
        // TODO maybe add a getVarF which doesn't return an Option
        val primesF = getVarF[Set[Int]]("primes").map(_.get)
        val pF = getVarF[Int]("p").map(_.get)

        val cond = for {
          primes <- primesF
          p <- pF
        } yield primes.contains(p)

        // TODO desperately needs a for loop construct
        val innerWhile = for {
          _ <- setVarF("i", pF.map(_ * 2))
          _ <- whileF(getVarF[Int]("i").map(_.get <= n)) {
            val iF = getVarF[Int]("i").map(_.get)
            for {
              _ <- setVarF("primes", primesF.flatMap(primes => iF.map(i => primes - i)))
              _ <- pF.flatMap(p => setVarF("i", iF.map(_ + p)))
            } yield ()
          }
        } yield ()

        for {
          _ <- partialCondF(cond -> innerWhile)
          _ <- setVarF("p", pF.map(_ + 1))
        } yield ()
      }
      result <- getVarF[Set[Int]]("primes")
    } yield result

    // Bombs with a StackOverflowError??
    sieve(20).foldMap(interpreter) should equal (Set(2,3,5,7,11,13,17,19))
  }

}
