package io.andrelucas
package errorhandling

import zio.{Scope, ZIO, ZIOAppArgs, ZIOAppDefault}

object ZIOErrorHandling extends ZIOAppDefault {

  val stringSize = (name: String) => ZIO.succeed(name.length)
  val myNameInSize = stringSize("Andre")

  val aFailedZIO = ZIO.fail("oh no!")
  val aFailedWithT = ZIO.fail(new RuntimeException("oh no!"))
  val aFailedWithDesc = aFailedWithT.mapError(_.getMessage)


  override def run: ZIO[Any with ZIOAppArgs with Scope, Any, Any] = ???

}
