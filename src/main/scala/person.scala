/* ------------------------------------------------------------
 !Sims person
 ------------------------------------------------------------ */
package tm8st.sims

import processing.core._
import scala.util._

import tm8st.util._
import tm8st.engine._
import tm8st.aigoal._

/* ------------------------------------------------------------
 !人の肉体的、精神的状態 object
 !@memo
 ------------------------------------------------------------ */
object PersonState
{
  // カーブ設定
  val splineHunger = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBladder = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBoke = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineTsukkomi = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineSocial = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineHp = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (0.5f, 0.0f), (1.f, 0.0f)))

  // パラメータの値域
  val paramMin = -100.f
  val paramMax = 100.f
  
  // 重み計算
  def calcHungerWeight(x:Float) = PersonState.splineHunger.map(Util.remap(x, paramMin, paramMax))
  def calcBladderWeight(x:Float) = PersonState.splineBladder.map(Util.remap(x, paramMin, paramMax))
  def calcBokeWeight(x:Float) = PersonState.splineBoke.map(Util.remap(x, paramMin, paramMax))
  def calcTsukkomiWeight(x:Float) = PersonState.splineTsukkomi.map(Util.remap(x, paramMin, paramMax))
  def calcSocialWeight(x:Float) = PersonState.splineSocial.map(Util.remap(x, paramMin, paramMax))
  def calcHpWeight(x:Float) = PersonState.splineHp.map(Util.remap(x, paramMin, paramMax))

  //
  def getParamDeclares() = 
  {
    List[(String, Float=>Float)](
      ("お腹", calcHungerWeight),
      ("便意", calcBladderWeight),
      ("ボケ", calcBokeWeight),
      ("ツッコミ", calcTsukkomiWeight),
      ("楽しさ", calcSocialWeight),
      ("体力", calcHpWeight),
       )
  }
}
/* ------------------------------------------------------------
 !人の肉体的、精神的状態
 !@memo
 ------------------------------------------------------------ */
case class PersonState(aHunger:Float, aBladder:Float, aBoke:Float, aTsukkomi:Float, aSocial:Float, aHp:Float)
{  
  val hunger = Util.clamp(aHunger, PersonState.paramMin, PersonState.paramMax)
  val bladder = Util.clamp(aBladder, PersonState.paramMin, PersonState.paramMax)
  val boke = Util.clamp(aBoke, PersonState.paramMin, PersonState.paramMax)
  val tsukkomi = Util.clamp(aTsukkomi, PersonState.paramMin, PersonState.paramMax)
  val social = Util.clamp(aSocial, PersonState.paramMin, PersonState.paramMax)
  val hp = Util.clamp(aHp, PersonState.paramMin, PersonState.paramMax)

  // 
  def affect(r:PersonState):PersonState =
    PersonState(hunger + r.hunger, bladder + r.bladder, boke + r.boke, tsukkomi + r.tsukkomi, social + r.social, hp + r.hp)

  // 
  def update(delta:Float) = 
    new PersonState(hunger - 1.f * delta, bladder - 1.f * delta, boke - 0.f * delta, tsukkomi - 0.f * delta, social - 1.f * delta, hp + 1.f * delta)
    // new PersonState(hunger - 0.001f * delta, bladder - 0.001f * delta, boke - 0.1f * delta, tsukkomi - 0.1f * delta, hp + 0.01f * delta)

  //
  def calcMode():Float =
  {
    PersonState.calcHungerWeight(hunger) * hunger +
    PersonState.calcBladderWeight(bladder) * bladder +
    PersonState.calcBokeWeight(boke) * boke +
    PersonState.calcTsukkomiWeight(tsukkomi) * tsukkomi +
    PersonState.calcSocialWeight(social) * social +
    PersonState.calcHpWeight(hp) * hp
  }
  //
  override def toString =
  {
    "PersonState:\n hunger " + hunger + "\n bladder " + bladder + "\n boke " + boke + "\n tsukkomi " + tsukkomi + "\n social " + social + "\n hp " + hp
  }
}
/* ------------------------------------------------------------
 !人物アクタ
 !@memo
 ------------------------------------------------------------ */
class APerson(name:String, var pos:Vector3, val world:World, val actions:List[Action], var actionChannel:Int) extends ActionTarget
{
  var state = PersonState(0.f, 0.f, 100.f, 100.f, 0.f, 100.f)
  var bounds = new Bounds(16.f)
  var walkSpeed = 50.f
  var mode = state.calcMode()

  // Action variable
  var actionCounter = 0.f
  var currentAction:Action = null
  var currentActionTarget:ActionTarget = null

  // AI Root
  val aiRoot = new PGRoot(this)
  aiRoot.activate()

  // Primitives
  val sphere = new CSpherePrimitive(pos, bounds)
  {
    strokeColor = new Color(128, 98, 98)
    setDrawPriority(1)
  }
  addPrimitive(sphere)

  val label = new CLabelPrimitive(name, pos, bounds)
  {
    fillColor = new Color(0, 0, 32)
    setDrawPriority(1)
  }
  addPrimitive(label)

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    if(currentAction != null)
    {
      actionCounter += delta
      if(actionCounter > 2.f)
	{
	  currentAction.Run(this)
	  world.addActor(new ASerif(currentAction.name, pos, world))

	  currentAction = null
	  currentActionTarget = null
	}
    }
    
    aiRoot.tick(delta)
    
    // State
    state = state.update(delta)
  }

  def startAction(actionTarget:ActionTarget, action:Action)
  {
    actionCounter = 0.f
    currentAction = action
    currentActionTarget = actionTarget
  }

  def isActionEnd() = currentAction == null
  def isCanAction(actionTarget:ActionTarget, action:Action) =
  {
    val dif = actionTarget.pos - pos;
    dif.size < (bounds.radius + actionTarget.bounds.radius)
  }
  def isReachable(actionTarget:ActionTarget) = true

  def moveToTarget(actionTarget:ActionTarget)
  {
    val dif = actionTarget.pos - pos;
    if(dif.size > (bounds.radius + actionTarget.bounds.radius))
      pos = pos + dif.normal() * walkSpeed * world.deltaTime
  }
  
 // if(nextAction == null)
    //   {
    // 	thinkNextAction()
    //   }
    // else
    //   {
    // 	val dif = actionTarget.pos - pos;
    // 	Logger.debug("move: " + name + " dif " + dif.toString + " difSize " + dif.size)
    // 	if(dif.size > (bounds.radius + actionTarget.bounds.radius))
    // 	  pos = pos + dif.normal() * walkSpeed * delta
    // 	else
    // 	  {
    // 	    nextAction.Run(this)
    // 	    world.addActor(new ASerif(nextAction.name, pos, world))
    // 	    nextAction = null
    // 	    actionTarget = null
    // 	  }
    //   }

  // 
  def ChangeState(effect:PersonState)
  {
    state = state.affect(effect)
  }

  // 
  override def toString() =
  {
    // var act = if(nextAction!= null) nextAction.name else "null"
    // act = " NextAction " + act + "\n"
    "APerson " + name + "\n" + " Mode " + state.calcMode().toString() + "\n" + state.toString() + "\n" + aiRoot.toString()
  }
}
/* ------------------------------------------------------------
   !ルートGoal
   !@memo
------------------------------------------------------------ */
class PGRoot(aOwner:APerson) extends AIGoalComposite[APerson](aOwner)
{
  override def toString() = "Root\n" + super.toString()

  // 
  override def activate()
  {
    super.activate()

    setActive()
  }

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    val ret = tickSubGoals(delta)
    if(ret == AIGoal.STATE_FINISHED)
    {
      thinkNextAction()
    }
  }

  // 
  def thinkNextAction()
  {
    Logger.debug("thinkNextAction: " + getOwner.name)

    var maxState = getOwner.state
    var bestAction:Action = null
    var bestActionTarget:ActionTarget = null
    for(at <- getOwner.simsWorld().getActionTargets(); a <- at.actions.filter(_.canDo(getOwner)))
    {
      if(at != this)
	{
	  var newState = getOwner.state.affect(a.effect)
	  Logger.debug("mode: new " + newState.calcMode() +", max " + maxState.calcMode() + " if " + (newState.calcMode() > maxState.calcMode()).toString)
	  if(newState.calcMode() > maxState.calcMode())
	    {
	      maxState = newState
	      bestAction = a
	      bestActionTarget = at
	    }
	}
    }

    if(bestAction != null)
    {
      addSubGoal(new PGAction(getOwner, bestAction, bestActionTarget))
      Logger.debug("Set Action: " + getOwner.name + " " + bestAction.name)
    }
  }
}
/* ------------------------------------------------------------
   !アクションGoal
   !@memo
------------------------------------------------------------ */
class PGAction(aOwner:APerson, val action:Action, val actionTarget:ActionTarget) extends AIGoalComposite[APerson](aOwner)
{
  override def toString() = "PGAction("+action.name+" target "+actionTarget.name+")\n" + super.toString()

  // 
  override def activate()
  {
    super.activate()

    addSubGoal(new PGActionRun(getOwner, action, actionTarget))
    addSubGoal(new PGActionMoveTarget(getOwner, action, actionTarget))

    setActive()
  }

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    val ret = tickSubGoals(delta)
    if(ret == AIGoal.STATE_FINISHED)
    {
      setFinished()
    }
  }
}
/* ------------------------------------------------------------
   !ターゲットへの移動Goal
   !@memo
------------------------------------------------------------ */
class PGActionMoveTarget(aOwner:APerson, val action:Action, val actionTarget:ActionTarget) extends AIGoalAtomic[APerson](aOwner)
{
  override def toString() = "PGActionMoveTarget("+action.name+" target "+actionTarget.name+")\n" + super.toString()

  var checkIntervalCounter = 0.f
  
  // 
  override def activate()
  {
    super.activate()

    checkIntervalCounter = 0.f
    if(getOwner.isReachable(actionTarget))
    {
      setActive()
    }
    else
    {
      setFailed()
    }
  }

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    checkIntervalCounter += delta

    getOwner.moveToTarget(actionTarget)
    if(getOwner.isCanAction(actionTarget, action))
    {
      setFinished()
    }
    else
    {
      if(checkIntervalCounter > 1.f)
	{
	  checkIntervalCounter = 0.f
	  if(getOwner.isReachable(actionTarget) == false)
	  {
	    setFailed()
	  }
	}
    }
  }
}
/* ------------------------------------------------------------
   !アクション実行Goal
   !@memo
------------------------------------------------------------ */
class PGActionRun(aOwner:APerson, val action:Action, val actionTarget:ActionTarget) extends AIGoalAtomic[APerson](aOwner)
{
  override def toString() = "PGActionRun("+action.name+" target "+actionTarget.name+")\n" + super.toString()

  var checkIntervalCounter = 0.f
  
  // 
  override def activate()
  {
    super.activate()

    checkIntervalCounter = 0.f
    if(getOwner.isCanAction(actionTarget, action))
    {
      getOwner.startAction(actionTarget, action)

      setActive()
    }
    else
    {
      setFailed()
    }
  }

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    checkIntervalCounter += delta

    if(getOwner.isActionEnd())
    {
      setFinished()
    }
  }
}
