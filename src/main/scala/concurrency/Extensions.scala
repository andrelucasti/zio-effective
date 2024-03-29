package io.andrelucas
package concurrency

import zio.ZIO

extension[R,E,A](zio: ZIO[R,E,A])
  def debugThread: ZIO[R,E,A] =
    zio
      .tap(a => ZIO.succeed(println(s"[${Thread.currentThread().getName}] $a")))
      .tapError(cause => ZIO.succeed(println(s"[ ${Thread.currentThread().getName} FAIL $cause]")))



