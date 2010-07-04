/* ------------------------------------------------------------
 !Sims memory
 ------------------------------------------------------------ */
package tm8st.sims

import scala.util._

import tm8st.util._
import tm8st.engine._
import tm8st.aigoal._

/* ------------------------------------------------------------
   !感想Enum
   !@memo
------------------------------------------------------------ */
object Feedback extends Enumeration
{
  val Fun, Sad, Good, NotBad, Bad, TooBad = Value
}
/* ------------------------------------------------------------
   !記憶の場所Enum
   !@memo
------------------------------------------------------------ */
object MemoryPlace extends Enumeration
{
  val Short, Shallow, Deep = Value
}
/* ------------------------------------------------------------
 !記憶クラス
 !@memo 見た事、聞いた事、やった事、感じた事などの記憶
 ------------------------------------------------------------ */
class Memory(val time:Float, val place:Vector3, val actActor:AActor, val action:Action,
	     val targetActor:ActionTarget, val feedback:Feedback.Value,
	     var strength:Float)
{
  override def toString() =
  {
    "Time " + time + " Act:" + action.toString() + " Feed:" + feedback.toString() +
    " Strength:" + strength.toString() + " Target:" + targetActor.name
  }
}
/* ------------------------------------------------------------
 !記憶の所有者
 !@memo
 ------------------------------------------------------------ */
abstract trait MemoryOwner
{
  // memory variables
  type Memories = List[Memory]
  
  var shortMemorys:Memories = List()
  var shallowMemorys:Memories = List()
  var deepMemorys:Memories = List()

  // 各記憶の容量
  def getShortMemoryMaxLength():Int
  def getShallowMemoryMaxLength():Int
  def getDeepMemoryMaxLength():Int

  //
  def addMemory(m:Memory)
  {
    shortMemorys ::= m

    if(shortMemorys.length > getShortMemoryMaxLength())
    {
      val maxStrength = shortMemorys.reduceLeft((a, b) => if(a.strength > b.strength) a else b)

      shortMemorys = shortMemorys.filterNot(_ == maxStrength)
      shallowMemorys ::= maxStrength
        
      if(shallowMemorys.length > getShallowMemoryMaxLength())
      {
        val maxStrength = shallowMemorys.reduceLeft((a, b) => if(a.strength > b.strength) a else b)

        shallowMemorys = shallowMemorys.filterNot(_ == maxStrength)
        deepMemorys ::= maxStrength

        if(deepMemorys.length > getDeepMemoryMaxLength())
        {
          val minStrength = deepMemorys.reduceLeft((a, b) => if(a.strength < b.strength) a else b)
          deepMemorys = deepMemorys.filterNot(_ == minStrength)
        }
      }
    }
  }

  // 
  def getInfo() =
  {
    "Memory:\n" +
    "Short:\n" + shortMemorys.map(_.toString() + "\n") + "\n" +
    "Shallow:\n" + shallowMemorys.map(_.toString() + "\n") + "\n" +
    "Deep:\n" + deepMemorys.map(_.toString() + "\n") + "\n"
    // "Short:\n" + maybe(shortMemorys.map(_.toString() + "\n"), reduceLeft(_ + _)) + "\n" +
    // "Shallow:\n" + shallowMemorys.map(_.toString() + "\n").reduceLeft(_ + _) + "\n" +
    // "Deep:\n" + deepMemorys.map(_.toString() + "\n").reduceLeft(_ + _) + "\n" +
  }
}
