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
                      val sum:Double, val avg:Double,
                      val dev:Double)
    {
      private def timeToMsec(t:Double) = t / 1000000.0

      def sumMsec() = timeToMsec(sum)
      def avgMsec() = timeToMsec(avg)
      def devMsec() = timeToMsec(dev)
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

      val item = Record(label, times, sum, avg, dev)
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
  private def format(label: String, reporter: Reporter) =
  {
    reporter.result.map { r =>
      val totalMsec = String.format("%05f", r.sumMsec(): java.lang.Double)
      var detail  = "  [" + r.label + "]"
      if(r.times.length > 1)
        detail += " x " + r.times.length + " "
      detail += " : Total " + totalMsec + " msec."

      if(r.times.length > 1)
      {
        val avgMsec = String.format("%05f", r.avgMsec(): java.lang.Double)
        detail += avgMsec.toString.mkString("\n   Avg. ", "", " msec.")
        val devMsec = String.format("%05f", r.devMsec(): java.lang.Double)
        detail += devMsec.toString.mkString("\n   Dev. ", "", " msec.")
      }

      detail
    }.mkString(label + "\n", "\n", "")
  }
}
