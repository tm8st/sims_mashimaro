import org.scalatest.FunSuite

import scala.collection.immutable._
import scala.collection.mutable.Stack

import org.scalatest.Spec
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import org.scalatest.FunSuite

import tm8st.util._
import tm8st.util.chararec._
import tm8st.util.benchmark._

/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class BenchmarkSuite extends FunSuite with ShouldMatchers
{
  private class MemFunTest
  {
    var a:Int = 0
    def fun(){ a += 1}
  }
  
  test("A Benchmark")
  {  
    def fib(n: Int): Int = if(n == 0) 1 else fib(n - 1)
    def ack(x: Int, y: Int): Int = x match {
      case 0 => y + 1
      case _ => y match { case 0 => ack(x - 1, 1); case _ => ack(x - 1, ack(x, y - 1))}
    }

    Benchmark.run("A Ack perf test") { b =>
      b.report("ack(3, 10)") { ack(3, 10) }
      b.report("ack(3, 10)", 10) { ack(3, 10) }
      None
   }

    Benchmark.run("A Math perf test") { b =>
      b.report("mul", 100) { (1 to 100).reduceLeft(_ * _) }
      b.report("add", 100) { (1 to 100).reduceLeft(_ + _) }
      None
   }

    var testObj = new MemFunTest()
    Benchmark.run("A MemFun perf test") { b =>
      b.report("memFun new", 100) { var t = new MemFunTest(); for(i <- 0 to 100) t.fun() }
      b.report("memFun scope", 100) { for(i <- 0 to 100) testObj.fun() }
      b.report("localVar", 100) { var t = 0; for(i <- 1 to 100) t += 1 }
      None
   }
  }
}
/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class UtilSpec extends Spec
{  
  describe("Util clamp")
  {
    it("clamp min")
    {
      expect(0.f){ Util.clamp(-1.f, 0.f, 1.f) }
    }
    it("clamp max")
    {
      expect(0.f){ Util.clamp(-1.f, 0.f, 1.f) }
    }
  }
}
/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class TestSuite extends FunSuite with ShouldMatchers
{
  test("""Util clamp""")
  {
    assert(Util.clamp(-1.f, 0.f, 1.f) == 0.f)
    assert(Util.clamp(3.f, 0.f, 1.f) == 1.f)
  }

  test("""Util remap""")
  {
    assert(Util.remap(3.f, 3.f, 6.f) == 0.f)
    assert(Util.remap(6.f, 3.f, 6.f) == 1.f)
    assert(Util.remap(4.5f, 3.f, 6.f) == 0.5f)
  }

  test("""Util line""")
  {
    assert(Util.getMaxLineLength(List("a", "bb", "ccc", "dddd", "eeeee")) == 5)
    assert(Util.getMaxLineLength(List("a", "bb", "ccc", "dddd", "eeeee")) != 0)

    assert(Util.getLineNum(List("a", "bb", "ccc", "dddd", "eeeee")) == 5)
  }

  test("""CharaRec""")
  {
    val cr = new CharacterRecognition()
    cr.learning(10, 10)
    cr.recognition(CharacterRecognition.SimplePatterns(0)) should equal (0)
    cr.recognition(CharacterRecognition.SimplePatterns(1)) should equal (1)
  }

  test("""Util newArray""")
  {
    def calcValue(i:Int) = i

    // val arr = Util.newArray(calcValue, 3)
    // println(arr.toString)
    // assert(arr.toString equals "Array(0.f, 1.f, 2.f, 3.f)")
    // assert(arr == Array(0, 1, 2, 3))
  }
}
