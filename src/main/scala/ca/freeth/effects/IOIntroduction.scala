package ca.freeth.effects

import cats.effect.IO

import scala.io.StdIn

object IOIntroduction {

  val ourFirstIO: IO[Int] = IO.pure(42) // should not have side effects, evaluated eagerly
  val aDelayedIO: IO[Int] = IO.delay({
    println("Producing an integer")
    42
  })

  val aDelayedIO2: IO[Int] = IO { // apply == delay
    println("delayed by apply")
    42
  }

  //  val shouldNotDoThis: IO[Int] = IO.pure({println("pls no"); 42})

  def smallProgram(): IO[Unit] = for {
    line1 <- IO(StdIn.readLine())
    line2 <- IO(StdIn.readLine())
    _ <- IO.delay(println(line1 + line2))
  } yield ()

  // mapN - combine IO effects as tuples

  import cats.syntax.apply.*

  val combined: IO[Int] = (ourFirstIO, ourFirstIO).mapN(_ + _)

  def smallProgram2(): IO[Unit] = (IO(StdIn.readLine()), IO(StdIn.readLine())).mapN(_ + _).map(println)

  /**
   * Exercises
   */

  // sequence two IOs and take result of last one
  def sequenceTakeLast[A, B](a: IO[A], b: IO[B]): IO[B] =
    a.flatMap(_ => b)

  def sequenceTakeLast2[A, B](a: IO[A], b: IO[B]): IO[B] =
    a *> b // andThen (ice cream operator)

  def sequenceTakeLast3[A, B](a: IO[A], b: IO[B]): IO[B] =
    a >> b // andThen with by-name call

  // sequence two IOs and take first result
  def sequenceTakeFirst[A, B](a: IO[A], b: IO[B]): IO[A] =
    a.flatMap(a => b.map(_ => a))

  def sequenceTakeFirst2[A, B](a: IO[A], b: IO[B]): IO[A] =
    a <* b

  // repeat an IO effect forever
  def forever[A](a: IO[A]): IO[A] = a.flatMap(_ => forever(a))

  def forever2[A](a: IO[A]): IO[A] = a >> forever2(a)

  def forever3[A](a: IO[A]): IO[A] = a *> forever3(a)

  def forever4[A](a: IO[A]): IO[A] = a.foreverM

  // convert an IO to a different type
  def convert[A, B](a: IO[A], value: B): IO[B] = a.map(_ => value)

  def convert2[A, B](a: IO[A], value: B): IO[B] = a.as(value)

  // discard value inside an IO and just return Unit
  def asUnit[A](a: IO[A]): IO[Unit] = a.flatMap(_ => IO(()))

  def asUnit2[A](a: IO[A]): IO[Unit] = a.map(_ => ())

  def asUnit3[A](a: IO[A]): IO[Unit] = a.void

  // fix stack recursion
  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def sumIO(n: Int): IO[Int] =
    if (n <= 0) IO(0)
    else IO.delay(n + sum(n - 1))

  // (hard) write a fibonacci IO that does NOT crash on recursion
  // hints: use recursion, ignore exponential complexity, use flatmap heavily
  def fibonacci(n: Int): IO[BigInt] =
    if (n == 0 || n == 1) IO.pure(n)
    else IO(fibonacci(n - 1)).flatMap { fib1 =>
      IO(fibonacci(n - 2)).flatMap { fib2 =>
        fib1.flatMap(one => fib2.map(two => one + two))
      }
    }

  def fibonacci2(n: Int): IO[BigInt] =
    if (n == 0 || n == 1) IO.pure(n)
    else for {
      last <- IO.defer(fibonacci2(n - 1))
      prev <- IO.defer(fibonacci2(n - 2))
    } yield last + prev

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global // "platform"
    // "end of the world"
    println(fibonacci(10).unsafeRunSync())
    println(fibonacci2(10).unsafeRunSync())
    //    forever3(IO {
    //      println("forever!")
    //      Thread.sleep(100)
    //    }).unsafeRunSync()
    //    println(sumIO(20000).unsafeRunSync())
  }

}
