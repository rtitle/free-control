# free-control

## What is it?

This project provides control structures for the free monad in Scala. See below for usage and examples.

## Usage
The control flow algebra can be composed into an existing free algebra as described in [cats documentation](http://typelevel.org/cats/datatypes/freemonad.html) (see section called `Composing Free monads ADTs`): 

For example, assuming an existing `MyAlgebra` and `MyInterpreter`:
```
import control.free._

type MyApp[A] = ControlFlowAppp[MyAlegbra, A]

def program(implicit A: MyAlegbra[MyApp], C: ControlFlow[MyApp]): Free[MyApp, ???] = {
  import A._, C._
  
  // your program goes here
}

// The control flow interpreter wraps the other interpreter(s). This is necessary for implementing some control flow features.
val interpreter: MyApp ~> Id = ControlFlowInterpreter(MyInterpreter)

// Run it!
program.foldMap(interpreter)
```

## Features
Here is a tour of the supported features. All of this code (and more) is tested in [ControlFlowSpec](https://github.com/rtitle/free-control/blob/master/src/test/scala/control/free/ControlFlowSpec.scala).

For the below examples, assume we have a `Counter` free monad with the following algebra:
```
sealed trait CounterA[A]
final case class Set(n: Int) extends CounterA[Unit]  // Sets the current value
final case class Add(n: Int) extends CounterA[Unit]  // Adds a value to the currently stored value
final case object Get extends CounterA[Int]          // Gets the currently stored value
final case object Reset extends CounterA[Unit]       // Sets the current value to 0
```
Note there is state associated with this algebra: it stores a single `Int` value.

Now let's see what we can do with our control flow structures.

### While loops
Loops while a condition is true.
```
for {
  _ <- whileF(get.map(_ < 100)) {
    add(1)
  }
  res <- get
} yield res
// returns 100
```

### Repeat loops
Repeats an action `n` times.
```
for {
  _ <- repeatF(constF(100)) {
    add(1)
  }
  res <- get
} yield res
// returns 100
```

### Conditionals
Conditionals are specified by calling `condF` with multiple cases, followed by the otherwise case. The first case which evaluates to true is executed, and no further cases are executed.
```
for {
  _ <- set(5)
  res <- condF(
    get.map(_ < 2)  -> constF("less than 2"),
    get.map(_ < 5)  -> constF("less than 5"),
    get.map(_ < 10) -> constF("less than 10"))(
    otherwise        = constF("greater than or equal to 10")
} yield res
// returns "less than 10"
```

### Functions
We can put some of the above concepts together and define a function for multiplication.
```
for {
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
// returns 15
```

### Scope
If your free monad contains state (as is the case with our counter), we can control state changes with scope. Note when you push a scope, that scope has access to state in its parent scope, but any state changes are local to the child scope.
```
for {
  _ <- set(1)
  _ <- pushScopeF
  _   <- add(42)
  a   <- get
  _ <- popScopeF
  b <- get
} yield (a, b)
// returns (43, 1)
```

### Mutable Variables
Sometimes scope is not sufficient to control state, and we want to store things in mutable variables. Variables are named, and can be set and read.
```
for {
  _ <- setVarF("v1", constF(3))
  _ <- setVarF("v2", constF(5))
  res1 <- getVarF[Int]("v1")
  res2 <- getVarF[Int]("v2")
  res3 <- getVarF[Int]("bogus")
} yield (res1, res2, res3)
// returns (Some(3), Some(5), None)
```

### Goto
What kind of control flow would we have without goto? This is just for fun, I don't recommend you use this (really this goes for the entire project, but more so goto).
```
for {
  _ <- set(100)
  _ <- labelF("lbl")
  _ <- add(-1)
  _ <- condF(
    get.map(_ > 10) -> gotoF("lbl"))(
    otherwise = stayF)
  res <- get
} yield res
// returns 10
```
