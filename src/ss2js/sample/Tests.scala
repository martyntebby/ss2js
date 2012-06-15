package ss2js.sample

import ss2js.global._

/** Testing helper */
class Assert {
  def assert(b: Boolean, msg: String = "") = if(!b) throw new Error(msg)
  def print(msg: Any): Unit = jsni("print(msg)")

  var testCount = 0
  var errorCount = 0

  /** enclose test case */
  def check[T](msg: String, expected: T = ())(body: => T) = {
    testCount += 1
    var ok = false
    print(msg + "   ")
    try {
      val ret = body
      ok = ret == expected
      if(ok)
          print("OK")
      else
        print("ERROR: expected: " + expected + "  received: " + ret)
    }
    catch { case e =>
      print("ERROR: expected: " + expected + "  exception: " + e)
    }
    if(!ok)
      errorCount += 1
    print("\n")
  }

  def printResults() {
    print(testCount + " tests   ")
    print(errorCount + " failures")
    print("\n")
    if(errorCount == 0) print("OK")
    else print("Failed")
  }
}

object Tests extends Assert {

  def tests() {
    ifTests(true)
    ifTests(false)
    loopTests()
    matchTests()
    tryCatchTest(true)
    tryCatchTest(false)
    classTests()
    classTests2()
    nestedTests()
    implicitTest()
    functionalTests()
    nativeTest()
    printResults()
  }

  def ifTests(b: Boolean) {
    check("if " + b) {
      if(b) assert(b, "if") else assert(!b, "if")
      val a = if(b) 1 else 2
      if(b) assert(a == 1, "condition") else assert(a == 2, "condition")
      if(!b) { 1; 2 } else { 3; 4 }
    }
  }

  def loopTests() {
    var i = 0;
    check("while", 0) {
      while(i > 0) {
        i = i - 1
        assert(false, "while")
      }
      i
    }
    check("do while", -1) {
      do i -= 1
      while(i > 0)
      i
    }

    class MyList {
      def foreach(meth: Int => scala.Unit) { meth(1) }
      def map(meth: Int => Int) = { meth(1) }
    }

    check("for comprehension") {
      val list = new MyList
      for(item <- list) { assert(item == 1, "for using foreach") }
      for(item <- list) yield { assert(item == 1, "for using map") ; item }
    }
  }

  def matchTests() {
    def test(a: String): Int = {
      a match {
        case "a" => return 1
        case "b"|"B" => return 2
        case "c" =>
        case _ => ()
      }
      0
    }
    check("match") {
      assert(test("a") == 1, "match a")
      assert(test("b") == 2, "match b")
      assert(test("B") == 2, "match B")
      assert(test("c") == 0, "match c")
    }
  }

  def tryCatchTest(doThrow: Boolean) {
    check("try catch finally " + doThrow) {
    var a = 0
    try {
      a = 1
      if(doThrow) throw new Error("try")
      assert(!doThrow, "after throw")
      a = 2
    }
    catch {
      case _ => { assert(a == 1, "catch"); a = 3 }
    }
    finally {
      if(doThrow)
        assert(a == 3, "finally")
      else  
        assert(a == 2, "finally")
      a = 4
    }
    assert(a == 4, "after finally")
    }
    val expected = if(doThrow) 2 else 1
    check("try expression " + doThrow, expected) {
      try {
        if(doThrow) throw new Error("try 2") else 1
      }
      catch {
        case _ => 2
      }
      finally "sd"
    }
  }

  def classTests() {
    
	/** traits are not translated, just used for type checking */
	trait Trait1 {
	  val d: Double
	  def func1: Int
//	  def func2() = func1
	}
	
	class Samplez(val a: Int) extends Trait1 {
	  val d = 1.1
	  override def func1 = a
	}
	
	class Sample2(a: Int) extends Samplez(a) {
	  val i = 1
	  var s = "abc"
	  type Int2 = Int
	  override def func1(): Int2 = { return 2; 3 }
	  def func3 = a
	  def superFunc1() = super.func1 + 2
	}
    
    check("class member") {
    val s = new Sample2(4)
    assert(s != null, "new object")
    assert(s.d == 1.1, "object double")
    assert(s.i == 1, "object integer")
    assert(s.s == "abc", "object string")
    s.s = "def"
    assert(s.s == "def", "assign object var")
    assert(s.func1 == 2, "override method")
//    assert(s.func2 == 2, "trait method")
    assert(s.func3 == 4, "return val method")
    assert(s.superFunc1 == 6, "super method")
    }
  }

  def classTests2() {
    class Outer(val a: Int) {
      var b = a
      override def toString() = "Outer " + a
      object Inner {
        def get() = b
        override def toString() = "Inner " + a
      }
    }
    check("inner and outer classes") {
      val o1 = new Outer(1)
      val g1 = o1.Inner.get
      val o2 = new Outer(2)
      val g2 = o2.Inner.get
      assert(g1 == 1, "1")
      assert(g2 == 2, "2")
    }
  }

  def nestedTests() {
    val a = 5
    class Nested {
      def f() = a      
    }
    class Nested2 extends Nested
    object Nested3 extends Nested2
    val n = new Nested2
    check("nested object and method") {
      assert(n.f == 5, "nested class")
      assert(Nested3.f == 5, "nested object")
      def nfunc() = "nested"
      assert(nfunc == "nested", "nested func")
    }
  }

  def implicitTest() {
    class RichString(val str: String) {
      def richStringMethod() = "rich" + str
    }
    implicit def str2rich(str: String) = new RichString(str)
    check("implicit method", "richabc") {
      "abc".richStringMethod()
    }
  }

  def functionalTests(b: Int = 2) {
    check("default param", 2) { b }
    def func(a: Int)(b: Int) = b
    check("multiple arg lists", 2) { func(1)(2) }
    def mycontrol(cont: Int => Int) = cont(3)
    check("closure", 6) { mycontrol { i => 3 + i } }
    
    class Cls(val a: Int) {
      def func(cls: Cls)(f: Cls => Int) = f(cls)
      def apply() = a
    }
    check("partially applied function", 2) {
      val cls1 = new Cls(1)
      val cls2 = new Cls(2)
      val a1 = cls1.func(cls2) _
      a1(_.a)
    }
    check("apply", 3) {
      val cls3 = new Cls(3)
      cls3()
    }
    check("no param method", 2) {
      def abc = 2
      abc
    }
    def abc2 { 2 }
  }

  def nativeTest() {
    def nativeFunc(): String = jsni("return 'native'")
    check("native javascript code", "native") {
      nativeFunc()
    }
  }

}

object Main {
  def main(args: scala.Array[String]) {
    Tests.tests()
  }
}
