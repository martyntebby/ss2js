package ss2js.sample

import ss2js.global._

/** Run minimal operations to test single piece of functionality */
object Simple {

  def assert(b: Boolean, msg: String = "") = { if(!b) throw new Error(msg) }
  def print(msg: Any): Unit = jsni("print(msg)")

  def main(args: scala.Array[String]) {
//    print("main")
    val a = new Def
    val b = a.func2
    assert(b == 1)
  }

}

  trait Abc {
    var a = 1
    def func(): Int
    def func2() = func
  }

  trait Abc2 extends Abc
  
  abstract class Def2 extends Abc
  class Def extends Def2 with Abc2 {
    val b = 2
    def func() = 1
  }
