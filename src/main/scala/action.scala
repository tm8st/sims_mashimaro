/* ------------------------------------------------------------
 !Sims Action.
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
 !行動クラス
 !@memo
 ------------------------------------------------------------ */
class Action(val name:String, val effect:PersonState, val effectTarget:PersonState,
             val channel:Int, val range:Float = 0.f, val time:Float = 1.f, val serifType:SerifType.Value = SerifType.Empty)
{
  def this(name:String, effect:PersonState, channel:Int) = this(name, effect, new PersonState, channel, 0.f, 1.f)
  def this(name:String, effect:PersonState, channel:Int, serifType:SerifType.Value) = this(name, effect, new PersonState, channel, 0.f, 1.f, serifType)

  override def toString() = name
  
  // //
  // def begin(person:APerson, actionTargets:Seq[ActionTarget])
  // {
  // }
  // //
  // def tick(person:APerson, actionTargets:Seq[ActionTarget])
  // {
  // }
  // //
  // def end(person:APerson, actionTargets:Seq[ActionTarget])
  // {
  // }

  // 
  def canDo(person:APerson):Boolean = (channel & person.actionChannel) > 0

  //
  def isSerifAction = serifType != SerifType.Empty

  //
  def Run(aActor:APerson)
  {
    if(canDo(aActor) == false)
    {
      Logger.error("invalid pair action: " + name + " " + aActor.name)
    }
    Logger.debug(name)
    
    if(isSerifAction)
    {
      aActor.changeState(effect)
      aActor.say(serifType, effectTarget)
    }
    else
    {
      aActor.changeState(effect)

      aActor.currentActionTargets.foreach(_.changeState(effectTarget))
    }
  }

  // 
  // def Run(aActor:APerson)
  // {
  //   if(canDo(aActor) == false)
  //   {
  //     Logger.error("invalid pair action: " + name + " " + aActor.name)
  //   }
    
  //   Logger.debug(name)

  //   aActor.changeState(effect)

  //   if(aActor.currentActionTarget)
  //   {
  //     aActor.currentActionTarget.changeState(effectTarget)
  //   }

  //   if(range > 0.f)
  //   {
  //     aActor.simsWorld().getPersons().
  //         filter(aActor.isIntersect(_)).
  //           map(_.changeState(effectTarget))
  //   }
  // }
}
