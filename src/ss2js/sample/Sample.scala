package ss2js.sample

import ss2js.global._

/**
 * Sample scala functionality that plugin handles.
 */
object Sample {

  def main(args: scala.Array[String]) {

    trait Trait1 {
      def func1(): Int
//      def func2(i: Int) = i
      val a: String
    }

    class Class1 extends Trait1 {
      def func1() = 1
      val a = "abc"
      var b = 3
    }

    class Class2 extends Class1 {
      override def func1() = super.func1()
    }

    def func2(d: Double = 1.1) = d
    func2()

    def func3(d: Double)(i: Int) = i
    func3(2.2)(2)

    val a = 1 match {
      case 1 => 2
      case 2 => 3
      case _ => 4
    }

    var b = try 1
    catch { case e => 2 }
    finally "not returned"

    object MyList {
      def foreach(meth: Int => Unit) { meth(1) }
      def map(meth: Int => Int) = { meth(1) }
    }
    for(item <- MyList) { }
    for(item <- MyList) yield item

    def apply(a: Int) {}
  }
}
