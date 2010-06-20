package tm8st.aigoal

import scala.util._

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
}
/* ------------------------------------------------------------
 !ゴールAIの抽象基底クラス
 !@memo
 ------------------------------------------------------------ */
abstract class AIGoal[TActor](aOwner:TActor)
{
  override def toString() = "AIGoal"

  type SelfType = AIGoal[TActor]

  private val owner:TActor = aOwner
  def getOwner() = owner
  
  //! 有効化
  def activate(){}
  //! 更新
  def tick(delta:Float){}
  //! 終了
  def terminate(){}

  //! 子ゴールの追加
  def addSubGoal(subGoal:SelfType){ assert(false); }
  //! 子ゴールの削除
  def removeSubGoal(subGoal:SelfType){}
  //! 子ゴールの全削除
  def removeSubGoals(){}

  // //! 現在実行中のAIリスト取得
  // def getActiveGoals(ILArray<AIGoal*>* pRet)
  // {
  //   if(IsActive()) pRet->push_back(this);

  //   return pRet;
  // }

  def isActive() = state == AIGoal.STATE_ACTIVE
  def isInActive() = state == AIGoal.STATE_INACTIVE
  def isFinished() = state == AIGoal.STATE_FINISHED
  def isFailed() = state == AIGoal.STATE_FAILED
  def isInit() = state == AIGoal.STATE_INIT

  def isNeedsRemove() = isFinished() || isFailed()

  def setActive(){ state = AIGoal.STATE_ACTIVE }
  def setInActive(){ state = AIGoal.STATE_INACTIVE }
  def setFinished(){ state = AIGoal.STATE_FINISHED }
  def setFailed(){ state = AIGoal.STATE_FAILED }

  def getState():Int = state

  private var state = AIGoal.STATE_INIT
}
/* ------------------------------------------------------------
 !複合ゴールAIの基底クラス
 !@memo
 ------------------------------------------------------------ */
abstract class AIGoalComposite[TActor](owner:TActor) extends AIGoal(owner)
{
  override def toString() =
  {
    var ret = ""
    val space = "a"

    // subGoals.reduceLeft()(_.toString() + _.toString())
    // subGoals.reduceLeft(AIGoal => String)(x => space * subGoals.indexOf(x) + x.toString() + "\n")

    // println(subGoals.reduceLeft((a, b) => a.toString() + "\n" + " " * ls.indexOf(b) + b.toString()))

    for(s <- subGoals)
      if(s.isActive())
    	ret += space * subGoals.indexOf(s) + s.toString() + "\n"
    
    ret

    // subGoals.filter(_.isActive()).reduceLeft("") + (_.toString() + _.toString() + "\n")
  }

  type BaseType = AIGoal[TActor]

  var subGoals = List[BaseType]()
  
  override def addSubGoal(subGoal:BaseType)
  {
    subGoals = subGoal :: subGoals;

    subGoal.activate();
  }
  override def removeSubGoal(subGoal:BaseType)
  {
    subGoal.activate();

    subGoal.terminate();
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
  override def toString() = "AIGoalAtomic"
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

  // !Test code
  val pawn = new TestGameActor("testPawn")
  val ai = new AIGoalRoot(pawn)

  ai.activate()

  for(i <- 0 to 300)
    ai.tick(1.f)
}
