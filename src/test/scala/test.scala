/* ------------------------------------------------------------
 !Test codes.
 ------------------------------------------------------------ */

import org.scalatest.FunSuite

import scala.collection.immutable._
import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.io.Source

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

    Benchmark.run("A Ack perf test") { r =>
      r.report("ack(3, 10)") { ack(3, 10) }
      r.report("ack(3, 10)", 10) { ack(3, 10) }
      None
   }

    Benchmark.run("A Math perf test") { r =>
      r.report("mul", 100) { (1 to 100).reduceLeft(_ * _) }
      r.report("add", 100) { (1 to 100).reduceLeft(_ + _) }
      None
   }

    var testObj = new MemFunTest()
    Benchmark.run("A MemFun perf test") { r =>
      r.report("memFun new", 100) { var t = new MemFunTest(); for(i <- 0 to 100) t.fun() }
      r.report("memFun scope", 100) { for(i <- 0 to 100) testObj.fun() }
      r.report("localVar", 100) { var t = 0; for(i <- 1 to 100) t += 1 }
      None
   }

    Benchmark.run("For / Filter")
    { r =>
      val l = (0 to 1000).toList
      r.report("fot if", 100)
      {
        var sum = 0.f
        for(i <- l)
        {
          if(i % 3 == 0)
            sum += i + 1.f
        }
        // println(sum)
      }
      r.report("filter", 100)
      {
        var sum = l.foldLeft(0.f){(a, b) => a + IF(b % 3 == 0)(b + 1)(0.f)}
      }
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
    assert(Util.getMaxLineLength(List("")) == 0)

    assert(Util.getLineNum(List("")) == 1)
    assert(Util.getLineNum(List("a", "bb", "ccc", "dddd", "eeeee")) == 5)
  }

  test("""CharaRec""")
  {
    val cr = new CharacterRecognition()
    cr.learning(10, 10)
    cr.recognition(CharacterRecognition.SimplePatterns(0)) should equal (0)
    cr.recognition(CharacterRecognition.SimplePatterns(1)) should equal (1)
  }

  test("""Map""")
  {
    var map = Map("key1"->"val1", "key2"->"val2")

    assert(map("key1") == "val1")

    // var map = Map("key1"->"val1", "key2"->"val2")
    // val m = new Map()
    // cr.learning(10, 10)
    // cr.recognition(CharacterRecognition.SimplePatterns(0)) should equal (0)
    // cr.recognition(CharacterRecognition.SimplePatterns(1)) should equal (1)
  }

  test("""List""")
  {
    var ls = List(1, 2, 3)

    assert(ls(2) == 3)
    assert(ls(0) == 1)

    for(i <- 0 to 100)
      assert(Util.iRand() % ls.length < 3)

    // var map = Map("key1"->"val1", "key2"->"val2")
    // val m = new Map()
    // cr.learning(10, 10)
    // cr.recognition(CharacterRecognition.SimplePatterns(0)) should equal (0)
    // cr.recognition(CharacterRecognition.SimplePatterns(1)) should equal (1)
  }

  test("""Util newArray""")
  {
    def calcValue(i:Int) = i

    // val arr = Util.newArray(calcValue, 3)
    // println(arr.toString)
    // assert(arr.toString equals "Array(0.f, 1.f, 2.f, 3.f)")
    // assert(arr == Array(0, 1, 2, 3))
  }

  test("""Source""")
  {
    // val source = Source.fromPath("test.txt").getLines("\n")
  }
}
