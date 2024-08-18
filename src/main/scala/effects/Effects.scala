package effects

import scala.concurrent.Future
import scala.io.StdIn
import scala.util.Try

object Effects {

  // functional programming
  // EXPRESSIONS
  def combine(a: Int, b:Int): Int = a + b

  // local reasoning = type signature describes the kind of computation that will be performed
  // referential transparency = ability to replace an expression with the value that it evaluates to

  val five = combine(2, 3)
  val fiveV2 = 2 + 3
  val fiveV3 = 5

  // not all expressions are RT(referential transparency)
  // example 1: printing
  val resultOfPrinting: Unit = println("Learning ZIO")
  val resultOfPrintingV2:Unit = () // not the same

  // example 2: changing the variable

  var anInt = 0
  val changingInt: Unit = (anInt = 42) // side effect
  val changingIntV2: Unit = () // not the same program

  // side effect are inevitable

  /**
  Effect properties
  - the type signature describes what KIND of computation it will perform
  - the type signature describes the type of VALUE that it will produce
  - if side effects are required, construction must be separate from the EXECUTION
   */

  /**
  Example Options = possibly absent values
  - type signature describes the kind of computation = a possibly absent value
  - type signature says the computation returns an A, if the computation does produce something
  - no side effects are needed

  => Option is an effect
   */
  val anOption: Option[Int] = Option(42)

  /**
  Example 2: Future
  - describes an asynchronous computation
  - produces a value of type A, if it finishes and it's successful
  - side effect are required, construction is NOT SEPARATE from execution

  => Future is NOT an effect
   */
  import scala.concurrent.ExecutionContext.Implicits.global
  val anFuture: Future[Int] = Future(42)

  /**
  Example 3: MyIO
  - describes a computation which might perform side effects
  - produces a values of type A if the computation is successful
  - side effects are required, construction IS SEPARATE from execution

  My IO IS AN EFFECT!
   */
  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO(() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]): MyIO[B] =
      MyIO(() => f(unsafeRun()).unsafeRun())
  }

  val anIOWithSideEffects: MyIO[Int] = MyIO(() => {
    println("producing effect")
    42
  })

  /**
   * Exercises - create some IO which
   * 1. measure the current time of the system
   * 2. measure the duration of a computation
   *  - use exercise 1
   *  - use map/flatMap combinations of MyIO
   *  3. read something from console
   *  4. print something in the console (e.g "what's your name"), than read , then print a welcome message
   */

    // 1
    val currentTime: MyIO[Long] = MyIO(() => System.currentTimeMillis())

    // 2
    def measure[A](computation: MyIO[A]): MyIO[(Long, A)] = for {
      startTime <- currentTime
      result    <- computation
      endTime   <- currentTime
    } yield (endTime - startTime, result)

  def measure_v2[A](computation: MyIO[A]): MyIO[(Long, A)] = {
    currentTime.flatMap { startTime =>
      computation.flatMap { result =>
        currentTime.map { endTime =>
          (endTime - startTime, result)
        }
      }
    }
  }
  // 3

  val readLine: MyIO[String] = MyIO(() => StdIn.readLine())
  def putStrLn(line: String): MyIO[Unit] = MyIO(() => println(line))

    // 4
  val program = for {
    _ <- putStrLn("what's your name?")
    name <- readLine
    _ <- putStrLn(s"Welcome to Rock the JVM, $name!")
  } yield ()

  /**
   * A simplified ZIO
   *
   */

  case class MyZIO[-R, +E, +A](unsafeRun: R => Either[E, A]) {
    def map[B](f: A => B): MyZIO[R, E, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => Right(f(v))
      })

    def flatMap[R1 <: R, E1 >: E, B](f: A => MyZIO[R1, E1, B]): MyZIO[R1, E1, B] =
      MyZIO(r => unsafeRun(r) match {
        case Left(e) => Left(e)
        case Right(v) => f(v).unsafeRun(r)
      })
  }

  def main(args: Array[String]) : Unit = {
    anIOWithSideEffects.unsafeRun()
    program.unsafeRun()

  }

}
