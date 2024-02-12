package io.andrelucas
package concurrency

import zio.{Fiber, ZIO, ZIOAppDefault}

object Fibers2 extends ZIOAppDefault {

  def zipFibers[E,A,B](fiber1: Fiber[E,A], fiber2: Fiber[E,B]): ZIO[Any, Nothing, Fiber[E,(A,B)]] = {
    val result = for {
      f1 <- fiber1.join
      f2 <- fiber2.join
    } yield (f1, f2)

    result.fork
  }

  def chainFibers[E, A](fiber1: Fiber[E, A], fiber2: Fiber[E, A]): ZIO[Any, Nothing, Fiber[E, A]] =
    fiber1.join
      .orElse(fiber2.join).fork



  override def run = ???

}
