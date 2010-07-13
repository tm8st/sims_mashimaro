/* ------------------------------------------------------------
 !Test Profiler codes.
 ------------------------------------------------------------ */

import scala.collection.immutable._
import scala.collection.mutable.Stack
import scala.collection.mutable.Map
import scala.io.Source

import org.scalatest.Spec
import org.scalatest.FlatSpec
import org.scalatest.FunSuite

import tm8st.util._

/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class ProfilerSpec extends Spec
{  
  describe("A Profiler")
  {
    it("auto simple")
    {
      expect(2) // Root + a
      {
        Profiler.beginFrame()
        Profiler.auto("a", "", Color.Black) {
        }
        Profiler.endFrame()

        Profiler.getRecordNum()
      }
    }

    it("auto complex")
    {
      expect(7) // Root + etc
      {
        Profiler.beginFrame()
        Profiler.auto("a", "", Color.Black) {
          Profiler.auto("b", "", Color.Black) {
            Profiler.auto("c", "", Color.Black) {
              Profiler.auto("d1", "", Color.Black) {
              }
              Profiler.auto("d2", "", Color.Black) {
                Profiler.auto("e", "", Color.Black) {
                }
              }
            }
          }
        }
        Profiler.endFrame()
        Profiler.getRecordNum()
      }
    }

    it("auto call function")
    {
      expect(4) // Root + etc
      {
        def fun()
        {
          Profiler.auto("b", "", Color.Black){}
        }
        Profiler.beginFrame()
        Profiler.auto("a", "", Color.Black) {
          fun()
          fun()
        }
        Profiler.endFrame()
        Profiler.getRecordNum()
      }
    }

    it("auto return")
    {
      expect(2) // Root + a
      {
        def fun()
        {
          Profiler.auto("a", "", Color.Black)
          {
          }
        }
        Profiler.beginFrame()
        if(true){fun()}
        Profiler.endFrame()
        Profiler.getRecordNum()
      }
    }
  }
}
