package io.andrelucas
package effects

import zio.{Runtime, Trace, UIO, Unsafe, ZIO}

import scala.annotation.tailrec

object ZIOEffects {
  def sequenceTakeLast[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    for {
      a <- zioa
      b <- ziob
    } yield b

  def sequenceTakeLastV2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa.flatMap(_ => ziob.map(b => b))

  def sequenceTakeLastV3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, B] =
    zioa *> ziob


  def sequenceTakeFirst[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    for {
      a <- zioa
      b <- ziob
    } yield a

  def sequenceTakeFirstV2[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    ziob.flatMap(_ => zioa.map(a => a))

  def sequenceTakeFirstV3[R, E, A, B](zioa: ZIO[R, E, A], ziob: ZIO[R, E, B]): ZIO[R, E, A] =
    ziob *> zioa


  def convert[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.map(_ => value)

  def convertV2[R, E, A, B](zio: ZIO[R, E, A], value: B): ZIO[R, E, B] =
    zio.as(value)


  def asUnit[R, E, A, B](zioa: ZIO[R, E, A]): ZIO[R, E, Unit] =
    convertV2(zioa, ())

  def asUnitV2[R, E, A, B](zioa: ZIO[R, E, A]): ZIO[R, E, Unit] =
    zioa.unit


  def badSum(n: Int): Int =
    if n == 0 then 0
    else
      n + badSum(n - 1) // will crash at the sum(200000....)

  def sum(n: Int): Int =
    @tailrec
    def loop(n: Int, acc: Int): Int =
      if n == 0 then acc
      else
        loop(n- 1, acc + n)

    loop(n, 0)


  def sumZIO(n: Int): UIO[Int] =
    ZIO.succeed {
      @tailrec
      def loop(n: Int, acc: Int): Int =
        if n == 0 then acc
        else
          loop(n - 1, acc + n)

      loop(n, 0)
    }


  def sumZIOV2(n: Int): UIO[Int] =
    if n == 0 then ZIO.succeed(0)
    else for {
      current <- ZIO.succeed(n)
      prev <- sumZIOV2(n - 1)

    } yield current + prev


  def fiboZIO(n: Int): UIO[BigInt] =
    ZIO.suspendSucceed {
      @tailrec
      def loop(n: Int, acc: Int): UIO[BigInt] =
        if n < 1 then ZIO.succeed(acc)
        else
          loop(n - 1, acc + n )

      loop(n, 0)
    }


  def main(args: Array[String]): Unit = {
    val runtime = Runtime.default
    implicit val trace: Trace = Trace.empty

    Unsafe.unsafe{ implicit u =>
      val firstZIO = ZIO.succeed{
        println("Computing first value")
        Thread.sleep(500)

        2
      }

      val secondZIO = ZIO.succeed {
        println("Computing second value")
        Thread.sleep(500)

        4
      }

//      println(runtime.unsafe.run(sequenceTakeLast(firstZIO, secondZIO))) //expected 4
//      println(runtime.unsafe.run(sequenceTakeLastV2(firstZIO, secondZIO))) //expected 4
//      println(runtime.unsafe.run(sequenceTakeLastV3(firstZIO, secondZIO))) //expected 4
//
//      println(runtime.unsafe.run(sequenceTakeFirst(firstZIO, secondZIO))) //expected 2
//      println(runtime.unsafe.run(sequenceTakeFirstV2(firstZIO, secondZIO))) //expected 2
//      println(runtime.unsafe.run(sequenceTakeFirstV3(firstZIO, secondZIO))) //expected 2


//      println(runtime.unsafe.run(convert(firstZIO, 333)))
//      println(runtime.unsafe.run(convertV2(firstZIO, 444)))

//      println(runtime.unsafe.run(asUnit(firstZIO)))
//      println(runtime.unsafe.run(asUnit(secondZIO)))

      println(runtime.unsafe.run(ZIO.succeed(sum(10))))
      println(runtime.unsafe.run(sumZIO(1000000)))
      println(runtime.unsafe.run(sumZIOV2(1000000)))
    }
  }

}