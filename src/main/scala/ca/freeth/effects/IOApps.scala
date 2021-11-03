package ca.freeth.effects

import cats.effect.{ExitCode, IO, IOApp}

import scala.io.StdIn

object IOApps {

  val program = for {
    line <- IO(StdIn.readLine())
    _ <- IO(println(s"You've just written: $line"))
  } yield ()
}

object FirstCEApp extends IOApp {

  import IOApps.*

  override def run(args: List[String]): IO[ExitCode] = program.as(ExitCode.Success)
}

object MySimpleApp extends IOApp.Simple {
  import IOApps.*
  override def run = program
}