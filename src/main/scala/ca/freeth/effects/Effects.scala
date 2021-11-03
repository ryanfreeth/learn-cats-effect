package ca.freeth.effects

object Effects {

  def combine(a: Int, b: Int): Int = a + b

  val five = combine(2, 3)

  // mostly just a lecture on referential transparency and side effect handling

  case class MyIO[A](unsafeRun: () => A) {
    def map[B](f: A => B): MyIO[B] =
      MyIO[B](() => f(unsafeRun()))

    def flatMap[B](f: A => MyIO[B]) =
      MyIO[B](() => f(unsafeRun()).unsafeRun())
  }

  /**
   * Exercises
   * 1. An IO which returns current time of system
   * 2. An IO which measures duration of computation
   * 3. An IO which prints to console
   * 4. An IO which reads line from console
   */

  // 1
  def currentTime(): MyIO[Long] = MyIO[Long](() => System.currentTimeMillis())

  // 2
  def measure[A](computation: MyIO[A]): MyIO[Long] =
    currentTime().flatMap { start =>
      computation.flatMap { _ =>
        currentTime().map { end =>
          end - start
        }
      }
    }

  def longRunning(): MyIO[Unit] =
    MyIO(() => Thread.sleep(1000))

  // 3
  def putStrLn(line: String): MyIO[Unit] =
    MyIO(() => println(line))

  // 4
  def readStrLn(): MyIO[String] =
    MyIO(() => scala.io.StdIn.readLine("Line!: "))

  def main(args: Array[String]): Unit = {
    val duration = measure(longRunning())
    println(duration.unsafeRun())

    duration.flatMap { dur =>
      putStrLn(dur.toString)
    }.unsafeRun()

    readStrLn().flatMap { line =>
      putStrLn(s"You said: $line")
    }.unsafeRun()
  }
}
