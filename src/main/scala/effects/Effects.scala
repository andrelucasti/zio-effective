package io.andrelucas
package effects

import java.util.UUID

object Effects {

  case class MyIO[A](f:() => A)
  case class MyIOMap[A](f:() => A){
    def map[B](map: A => B): MyIOMap[B] = MyIOMap(() => map(f()))
    def flatMap[B](flatMap: A => MyIOMap[B]): MyIOMap[B] = MyIOMap(()=> flatMap(f()).f() )
  }

  val hello = MyIO(() => {
    "Hello !!!"
  })

  val numbersList = MyIO(()=> {
    Seq(1,2,3,4)
  })


  val stringSize: String => Int = (name: String) => MyIOMap(()=> name).map(_.length).f()



  //1. Current time of the system:
  val currentTime =  MyIO(()=> System.currentTimeMillis())

  def main(args: Array[String]): Unit = {
    println(hello.f())
    println(numbersList.f())
    println(stringSize("Andre"))
    println(stringSize("LucasSSSS"))

    println(currentTime.f())
  }
}
