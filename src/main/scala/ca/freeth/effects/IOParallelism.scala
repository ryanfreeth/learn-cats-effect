package ca.freeth.effects

import ca.freeth.utils.*
import cats.Parallel
import cats.Parallel.*
import cats.effect.{IO, IOApp}
import cats.syntax.apply.*
import cats.effect.implicits.*

object IOParallelism extends IOApp.Simple {

  val oneIO = IO(s"[${Thread.currentThread().getName}] One")
  val twoIO = IO(s"[${Thread.currentThread().getName}] Two")

  val composed = for {
    one <- oneIO
    two <- twoIO
  } yield s"$one and $two be sequential"



  val meaningOfLife = IO.delay(42).debug
  val favLang = IO.delay("Scala").debug
  val goalInLife = (meaningOfLife, favLang).mapN((num, string) => s"my goal in life is $num and $string")

  // parallelism in IOs
  // convert sequential IO to parallel IO
  val parIO1: IO.Par[Int] = Parallel[IO].parallel(meaningOfLife.debug)
  val parIO2: IO.Par[String] = Parallel[IO].parallel(favLang.debug)
  val goalInLifePar = (parIO1, parIO2).mapN((num, string) => s"my goal in life is $num and $string")
  // turn back to sequential
  val goalInLife2: IO[String] = Parallel[IO].sequential(goalInLifePar)

  // shorthand:
  import cats.syntax.parallel.*
  val goalInLife3: IO[String] = (meaningOfLife, favLang).parMapN((num, string) => s"my goal in life is $num and $string")

  // what happens in failure?
  val aFailure: IO[String] = IO.raiseError(new RuntimeException("oopsie"))
  // compose a success and failure
  val parallelFailure = (meaningOfLife.debug, aFailure.debug).parMapN(_ + _)

  // compose failure + failure
  val anotherFailure: IO[String] = IO.raiseError(new RuntimeException("Not again!"))
  val twoFailures: IO[String] = (aFailure, anotherFailure).parMapN(_ + _)



  override def run: IO[Unit] =
    twoFailures.debug.void
}
