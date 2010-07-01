/* ------------------------------------------------------------
 !GoalAI lib.
 ----------------------------------------------------------- */
package tm8st.aigoal

import scala.util._
import tm8st.util._

/* ------------------------------------------------------------
 !ゴールAI周りで使用する定数等の定義
 !@memo
 ------------------------------------------------------------ */
object AIGoal
{
  // AIの実行状態
  val STATE_ACTIVE = 0
  val STATE_INACTIVE = 1
  val STATE_FINISHED = 2
  val STATE_FAILED = 3
  val STATE_INIT = 4

  private val stateID2StringTable = List("Active", "InActive", "Finished", "Failed", "Init")
  
  def stateID2String(id:Int) = 
    stateID2StringTable(id)
}
/* ------------------------------------------------------------
 !ゴールAIの抽象基底クラス
 !@memo
 ------------------------------------------------------------ */
abstract class AIGoal[TActor](aOwner:TActor)
{
  def name() = "AIGoal"
  override def toString() = name() + " " + AIGoal.stateID2String(state)
  
  def getDepth():Int =
  {
    val d = if(parent != null) parent.getDepth() else 0
    1 + d
  }
  
  type SelfType = AIGoal[TActor]

  private var parent:SelfType = null
  def setParent(p:SelfType){ parent = p }
  private val owner:TActor = aOwner

  def getOwner() = owner
  
  //! 有効化
  def activate()
  {
    Logger.debug(name + " activate.")
  }
  //! 更新
  def tick(delta:Float)
  {
    Logger.debug(name + " tick.")
  }
  //! 終了
  def terminate()
  {
    Logger.debug(name + " terminate.")
  }

  //! 子ゴールの追加
  def addSubGoal(subGoal:SelfType){ assert(false); }
  //! 子ゴールの削除
  def removeSubGoal(subGoal:SelfType){}
  //! 子ゴールの全削除
  def removeSubGoals(){}

  def isActive() = state == AIGoal.STATE_ACTIVE
  def isInActive() = state == AIGoal.STATE_INACTIVE
  def isFinished() = state == AIGoal.STATE_FINISHED
  def isFailed() = state == AIGoal.STATE_FAILED
  def isInit() = state == AIGoal.STATE_INIT

  def isNeedsRemove() = isFinished() || isFailed()

  protected def setActive(){ state = AIGoal.STATE_ACTIVE }
  protected def setInActive(){ state = AIGoal.STATE_INACTIVE }
  protected def setFinished(){ state = AIGoal.STATE_FINISHED }
  protected def setFailed(){ state = AIGoal.STATE_FAILED }

  def getState():Int = state

  private var state = AIGoal.STATE_INIT
}
/* ------------------------------------------------------------
 !複合ゴールAIの基底クラス
 !@memo
 ------------------------------------------------------------ */
abstract class AIGoalComposite[TActor](owner:TActor) extends AIGoal(owner)
{
  override def name() = "AIGoalComposite"
  override def toString() =
  {
    var offBase = getDepth()
    var space = "-"
    var ret = ""
    for(g <- subGoals)
    {
      ret += "\n" + space * offBase + g.toString()
    }
    super.toString() + ret

    // super.toString() + subGoals.filter(_.isActive()).map("\n " + _.toString())
  }

  type BaseType = AIGoal[TActor]

  var subGoals = List[BaseType]()
  
  override def addSubGoal(subGoal:BaseType)
  {
    subGoals = subGoal :: subGoals
    subGoal.setParent(this)
  }
  override def removeSubGoal(subGoal:BaseType)
  {
    subGoal.terminate()
    subGoal.setParent(null)
    subGoals -= subGoal
  }
  override def removeSubGoals()
  {
    subGoals.map(_.terminate())
    subGoals = List()
  }
  def tickSubGoals(delta:Float):Int =
  {
    // 終了したものを削除
    val prevLen = subGoals.length
    for(g <- subGoals)
      if(g.isNeedsRemove)
	removeSubGoal(g)
    // subGoals = subGoals.filter(_.isNeedsRemove() == false)
    // subGoals = subGoals.filter(_.isFinished() == false && _.isFailed() == false)
    
    // 更新
    var ret = AIGoal.STATE_ACTIVE
    if(subGoals.isEmpty == false)
    {
      val hd = subGoals.head
      if(prevLen != subGoals.length)
	hd.activate()

      hd.tick(delta)
      if(hd.isNeedsRemove() && subGoals.length == 1)
	ret = AIGoal.STATE_FINISHED
      else
	ret = AIGoal.STATE_ACTIVE
    }
    else
    {
      ret = AIGoal.STATE_FINISHED
    }

    ret
  }
};
/* ------------------------------------------------------------
 !単純ゴールAIの基底クラス
 !@memo
 ------------------------------------------------------------ */
abstract class AIGoalAtomic[TActor](owner:TActor) extends AIGoal(owner)
{
  override def name() = "AIGoalAtomic"
}
/* ------------------------------------------------------------
   !テストコード
   !@memo
------------------------------------------------------------ */
class TestAIGoal
{
  /* ------------------------------------------------------------
   !
   !@memo
   ------------------------------------------------------------ */
  class TestGameActor(val name:String)
  {
    var tire = 0.f
  }
  /* ------------------------------------------------------------
   !
   !@memo
   ------------------------------------------------------------ */
  class AIGoalRoot(owner:TestGameActor) extends AIGoalComposite(owner)
  { 
    override def activate()
    {
      super.activate()

      setActive()
    }
    override def tick(delta:Float)
    {
      super.tick(delta)

      val ret = tickSubGoals(delta)
      if(ret == AIGoal.STATE_FINISHED)
	{
	  if(owner.tire < 10)
	    {
	      println("root: let's work.")
	      addSubGoal(new AIGoalWork(owner))
	    }
	  else
	    {
	      println("root: let's sleep.")
	      addSubGoal(new AIGoalSleep(owner))
	    }
	}
      
      println("root: ...")
    }
  }
  /* ------------------------------------------------------------
   !
   !@memo
   ------------------------------------------------------------ */
  class AIGoalWork(owner:TestGameActor) extends AIGoalAtomic(owner)
  {
    var time = 10.f
    
    override def activate()
    {
      super.activate()

      setActive()
    }

    override def tick(delta:Float)
    {
      super.tick(delta)

      println("work: ... " + time)
      owner.tire += 1.f * delta
      time -= delta
      if(time < 0.f)
	{
	  println("work: end ")
	  setFinished()
	}
    }
  }
  /* ------------------------------------------------------------
   !眠る
   !@memo
   ------------------------------------------------------------ */
  class AIGoalSleep(owner:TestGameActor) extends AIGoalAtomic(owner)
  {
    var time = 3.f
    
    override def activate()
    {
      super.activate()

      setActive()
    }

    override def tick(delta:Float)
    {
      super.tick(delta)

      println("sleep: ... " + time)
      owner.tire = Math.max(owner.tire-1.f, 0.f)
      time -= delta
      if(time < 0.f)
	{
	  println("sleep: end ")
	  setFinished()
	}
    }
  }

  def main(args: Array[String])
  {
    // !Test code
    val pawn = new TestGameActor("testPawn")
    val ai = new AIGoalRoot(pawn)

    ai.activate()

    for(i <- 0 to 300)
      ai.tick(1.f)
  }
}
