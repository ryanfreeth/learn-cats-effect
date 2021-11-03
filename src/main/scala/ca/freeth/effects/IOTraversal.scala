package ca.freeth.effects

import cats.effect.IOApp
import scala.concurrent.Future
import scala.util.Random
import cats.Traverse
import cats.effect.IO

object IOTraversal extends IOApp.Simple {

  import scala.concurrent.ExecutionContext.Implicits.global

  def heavyComputation(string: String): Future[Int] = Future {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }

  val workload: List[String] = List(
    "I quite like CE",
    "Scala is great",
    "Looking forward to learning awesome stuff"
  )

  def clunkyFutures(): Unit = {
    val futures: List[Future[Int]] = workload.map(heavyComputation)
    futures.foreach(_.foreach(println))
  }

  import cats.Traverse
  import cats.instances.list._
  val listTraverse = Traverse[List]

  def traverseFutures(): Unit = {
    val singleFuture: Future[List[Int]] = listTraverse.traverse(workload)(heavyComputation)

    singleFuture.foreach(println)
  }

  import ca.freeth.utils.debug
  def computeAsIO(string: String): IO[Int] = IO {
    Thread.sleep(Random.nextInt(1000))
    string.split(" ").length
  }.debug

  val ios: List[IO[Int]]      = workload.map(computeAsIO)
  val singleIO: IO[List[Int]] = listTraverse.traverse(workload)(computeAsIO)

  // parallel traversal
  import cats.syntax.parallel._
  val parSingleIO: IO[List[Int]] = workload.parTraverse(computeAsIO)


  /**
   * Exercises
   */
  import cats.implicits._

  def sequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = 
      listOfIOs.traverse(identity)

  def sequence2[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = 
      Traverse[F].traverse(listOfIOs)(identity)

  def parSequence[A](listOfIOs: List[IO[A]]): IO[List[A]] = 
      listOfIOs.parTraverse(identity)

  def parSequence2[F[_] : Traverse, A](listOfIOs: F[IO[A]]): IO[F[A]] = 
      listOfIOs.parTraverse(identity)

  // existing sequence api
  val singleIO2 = listTraverse.sequence(ios)

  // parallel sequencing
  val parSingleIO2: IO[List[Int]] = ios.parSequence


  override def run = 
      parSingleIO2.map{_.sum}.debug.void
}
