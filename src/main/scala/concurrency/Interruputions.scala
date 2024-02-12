package io.andrelucas
package concurrency

import zio.test.Result.Succeed
import zio._

object Interruputions extends ZIOAppDefault {
  val testTimeoutZio =
    ZIO.succeed("Starting Computing").debugThread *>
      ZIO.sleep(2.seconds) *>
        ZIO.succeed("Finished").debugThread




  def timeout[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,A] =
    for {
      fib <- zio.fork
      _ <- ZIO.sleep(time) *> fib.interruptFork
      result <- fib.join
    } yield result


  def timeout2[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,Option[A]] =
    for {
      fib <- zio.fork
      _ <- ZIO.sleep(time) *> fib.interruptFork

      result <- fib.join
    } yield Option(result)


  def timeout3[R,E,A](zio: ZIO[R,E,A], time: Duration): ZIO[R,E,Option[A]] =
    timeout(zio, time).foldCauseZIO(
      cause => if cause.isInterrupted then ZIO.succeed(None) else ZIO.failCause(cause),
      value => ZIO.some(value)
    )

  override def run = timeout3(testTimeoutZio, 1.seconds).debugThread

}
