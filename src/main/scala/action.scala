/* ------------------------------------------------------------
 !Sims Action
 ------------------------------------------------------------ */
package tm8st.sims

import processing.core._
import scala.util._

import tm8st.util._
import tm8st.engine._
import tm8st.aigoal._
import tm8st.sims._

/* ------------------------------------------------------------
 !行動データ object
 !@memo
 ------------------------------------------------------------ */
object Action
{
  val ChannelBoke = 0x01
  val ChannelTsukkomi = 0x02
  val ChannelBisyoujo = 0x04
  val ChannelMatsuri = 0x08
  val ChannelOyaji = 0x10
  val ChannelUsual = 0x20
  val ChannelAll = 0xff
}
/* ------------------------------------------------------------
 !行動データ
 !@memo
 ------------------------------------------------------------ */
class Action(val name:String, val effect:PersonState, val channel:Int)
{
  // 
  def canDo(person:APerson):Boolean = (channel & person.actionChannel) > 0

  // 
  def Run(aActor:APerson)
  {
    if(canDo(aActor) == false)
      Logger.error("invalid pair action: " + name + " " + aActor.name)
    
    Logger.debug(name)
    aActor.ChangeState(effect)
  }
}
