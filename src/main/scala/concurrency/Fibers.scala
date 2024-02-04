package io.andrelucas
package concurrency

import zio.{Fiber, ZIO, ZIOAppDefault}

object Fibers extends ZIOAppDefault{

  // Fibers is a lightweight thread.

  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E,(A,B)]] =
    val finalEffect = for {
      v1 <- fiber1.join
      v2 <- fiber2.join
    } yield (v1, v2)
    finalEffect.fork



  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] =
    fiber1.join
      .orElse(fiber2.join)
      .fork

  def generateRandomFile(path: String):Unit = {

  }

  val zipFibersTest = for {
    fib1 <- ZIO.succeed("Result from fb1").debugThread.fork
    fib2 <- ZIO.succeed("Result from fb2").debugThread.fork
    fibers <- zipFibers(fib1, fib2)
    tuple <- fibers.join
  } yield tuple

  override def run = zipFibersTest.debugThread

}
