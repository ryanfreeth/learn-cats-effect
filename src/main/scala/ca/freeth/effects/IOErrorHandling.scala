package ca.freeth.effects

import cats.effect.IO

import scala.util.{Failure, Success, Try}

object IOErrorHandling {

  // we know: pure, delay, defer
  // learn: failed effects

  val aFailedCompute: IO[Int] = IO.delay(throw new RuntimeException("uh oh"))
  val aFailure: IO[Int] = IO.raiseError(new RuntimeException("a proper fail"))

  val dealWithIt = aFailure.handleErrorWith {
    case _: RuntimeException => IO.delay(println("I'm still here"))
  }

  val effectAsEither: IO[Either[Throwable, Int]] = aFailure.attempt

  val resultAsString = aFailure.redeem(ex => s"failed: $ex", value => s"Success: $value")

  val resultAsEffect = aFailure.redeemWith(ex => IO(println(s"failed: $ex")), value => IO(println(s"Success: $value")))

  /**
   * Exercises
   */

  // 1 - construct potentially failed IOs from Option, Try, Either
  def option2IO[A](op: Option[A])(ifEmpty: Throwable): IO[A] = op match {
    case Some(value) => IO(value)
    case _ => IO.raiseError(ifEmpty)
  }

  def try2IO[A](op: Try[A]): IO[A] = op match {
    case Failure(exception) => IO.raiseError(exception)
    case Success(value) => IO(value)
  }

  def either2IO[A](op: Either[Throwable, A]): IO[A] = op match {
    case Left(value) => IO.raiseError(value)
    case Right(value) => IO(value)
  }

  // 2 - handleError, handleErrorWith
  def handleIOError[A](io: IO[A])(handler: Throwable => A): IO[A] = io.redeem(handler, identity)

  def handleIOErrorWith[A](io: IO[A])(handler: Throwable => IO[A]): IO[A] = io.redeemWith(handler, IO.pure)

  def main(args: Array[String]): Unit = {
    import cats.effect.unsafe.implicits.global
    println(resultAsEffect.unsafeRunSync())
  }
}
