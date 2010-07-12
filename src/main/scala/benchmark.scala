package tm8st.util.benchmark

import java.io.{OutputStream, PrintStream, FileOutputStream}
import java.lang.System
import scala.collection.mutable.{Map, HashMap, ListBuffer, Set}
import scala.math._

/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
object Benchmark
{
  var ostream = System.out

  //  
  sealed class Reporter
  {
    type Times = ListBuffer[Double]
    case class Record(val label:String, val times:Times,
                      val sum:Double, val avg:Double, val dev:Double,
                      val minT:Double, val maxT:Double)
    {
      private def timeToMsec(t:Double) = t / 1000000.0

      def sumMsec() = timeToMsec(sum)
      def avgMsec() = timeToMsec(avg)
      def devMsec() = timeToMsec(dev)
      def minMsec() = timeToMsec(minT)
      def maxMsec() = timeToMsec(maxT)
    }
    var result = new ListBuffer[Record]()

    // public API
    def report(mult:Int)(block: => Unit): Unit = report("anonymous", mult)(block)
    def report(label:String)(block: => Unit): Unit = report(label, 1)(block)
    def report(label:String, mult:Int)(block: => Unit): Unit =
    {
      assert(mult > 0)

      var v:Any = null
      val times = new Times()
      for(i <- 0 to mult-1)
      {
        val start = System.nanoTime()
        v = block // XXX: disable effect of optimization
        val stop = System.nanoTime()
        times += (stop - start)
      }

      val sum = times.reduceLeft(_ + _)
      val avg = sum / mult
      val dev = sqrt(times.foldLeft(0.0)((sum,x) => sum + (x-avg) * (x-avg)) / mult)
      // Float.Max みたいなのにすべき
      val minT = times.foldLeft(1000000000.0)(min(_, _))
      val maxT = times.foldLeft(0.0)(max(_, _))

      val item = Record(label, times, sum, avg, dev, minT, maxT)
      result += item
    }
  }

  // run the benchmark test
  def run(label:String)(action:Reporter => Unit) =
  {
    val reporter = new Reporter
    action(reporter)
    print0(ostream, format("["+label+"]", reporter))
  }

  // output result
  private def print0(ostream: PrintStream, result: String) = ostream.println(result)
  private def print0(ostream: FileOutputStream, result: String) = ostream.write(result.toArray.map(_.toByte))
  private def formatTime(t:Double) = String.format("%05f", t: java.lang.Double)
  private def format(label: String, reporter: Reporter) =
  {
    reporter.result.map { r =>
      val totalMsec = formatTime(r.sumMsec())
      var detail  = "  [" + r.label + "]"
      if(r.times.length > 1) detail += " x " + r.times.length
      detail += " : Total " + totalMsec + " msec."

      if(r.times.length > 1)
      {
        detail += formatTime(r.avgMsec()).toString.mkString("\n   Avg. ", "", " msec.")
        detail += formatTime(r.devMsec()).toString.mkString("   Dev. ", "", " msec.")
        detail += formatTime(r.minMsec()).toString.mkString("\n   Min. ", "", " msec.")
        detail += formatTime(r.maxMsec()).toString.mkString("   Max. ", "", " msec.")
      }

      detail
    }.mkString(label + "\n", "\n", "")
  }
}
