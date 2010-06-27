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
  // パラメータの値域
  val paramMin = -100.f
  val paramMax = 100.f

  // カーブ設定
  val splineHunger = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBladder = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBoke = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineTsukkomi = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineSocial = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineHp = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (0.5f, 0.0f), (1.f, 0.0f)))

  // 重み計算
  def calcWeight(x:Float, curve:Spline) = curve.map(Util.remap(x, paramMin, paramMax))

  // 
  def calcHungerWeight(x:Float) = calcWeight(x, PersonState.splineHunger)
  def calcBladderWeight(x:Float) = calcWeight(x, PersonState.splineBladder)
  def calcBokeWeight(x:Float) = calcWeight(x, PersonState.splineBoke)
  def calcTsukkomiWeight(x:Float) = calcWeight(x, PersonState.splineTsukkomi)
  def calcSocialWeight(x:Float) = calcWeight(x, PersonState.splineSocial)
  def calcHpWeight(x:Float) = calcWeight(x, PersonState.splineHp)

  // 
  def getParamDeclares() = 
  {
    List[(String, Float=>Float)](
      ("お腹", calcHungerWeight),
      ("便意", calcBladderWeight),
      ("ボケ", calcBokeWeight),
      ("ツッコミ", calcTsukkomiWeight),
      ("人恋しさ", calcSocialWeight),
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
  // 状態変数
  //??? val states = List(aHunger, aBladder, aBoke, aTsukkomi, aSocial, aHp)
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
    "PersonState:\n hunger " + hunger +
    "\n bladder " + bladder +
    "\n boke " + boke +
    "\n tsukkomi " + tsukkomi +
    "\n social " + social +
    "\n hp " + hp
  }
}
/* ------------------------------------------------------------
 !人物アクタ
 !@memo
 ------------------------------------------------------------ */
class APerson(val personName:String, var pos:Vector3, val world:World, val actions:List[Action], var actionChannel:Int) extends ActionTarget with AActor
{
  override def gameObjectName = "APerson"
  override def name = gameObjectName + ":" + personName

  // Primitives
  var bounds = new Bounds(16.f)
  val sphere = new CSpherePrimitive(Vector3.Zero, bounds)
  {
    strokeColor = new Color(128, 98, 98)
    setDrawPriority(1)
  }
  addPrimitive(sphere)

  val label = new CLabelPrimitive(personName, Vector3.Zero, bounds, SimsGame.getFont(0))
  {
    fillColor = new Color(0, 0, 32)
    setDrawPriority(1)
  }
  addPrimitive(label)

  var state = PersonState(0.f, 0.f, 100.f, 100.f, 0.f, 100.f)
  private var walkSpeed = 50.f
  private var mode = state.calcMode()

  // Action variable
  var actionCounter = 0.f
  var currentAction:Action = null
  var currentActionTarget:ActionTarget = null

  // AI Root
  val aiRoot = new PGRoot(this)
  aiRoot.activate()

  // 
  override def tick(delta:Float)
  {
    Profiler.auto("Person Tick", name, Color.Black)
    {
      super.tick(delta)

      if(currentAction != null)
	{
	  actionCounter += delta
	  if(actionCounter > currentAction.time)
      	    {
      	      Logger.debug(name + " Run Action " + currentAction.name)
	      
      	      currentAction.Run(this)
      	      world.addActor(new ASerif(personName + ">" + currentAction.name, pos, world))
	      
      	      actionCounter = 0.f
      	      currentAction = null
      	      currentActionTarget = null
      	    }
	}
      
      aiRoot.tick(delta)
      
      state = state.update(delta)
    }
  }

  // 
  def startAction(actionTarget:ActionTarget, action:Action)
  {
    Logger.debug(name + " Start Action " + action.name)

    actionCounter = 0.f
    currentAction = action
    currentActionTarget = actionTarget
  }
  // 
  def isActionEnd() = currentAction == null
  // 
  def isCanAction(actionTarget:ActionTarget, action:Action):Boolean =
  {
    if(action.canDo(this) == false) return false

    val dif = actionTarget.pos - pos;
    dif.size < (bounds.radius + actionTarget.bounds.radius)
  }
  // 
  def isReachable(actionTarget:ActionTarget) = true
  // 
  def moveToTarget(actionTarget:ActionTarget)
  {
    val dif = actionTarget.pos - pos;
    if(dif.size > (bounds.radius + actionTarget.bounds.radius))
      pos = pos + dif.normal() * walkSpeed * world.deltaTime
  }
  // 
  def ChangeState(effect:PersonState)
  {
    state = state.affect(effect)
  }

  // 
  override def toString() =
  {
    "APerson:" + personName + "\n" + " Mode " + state.calcMode().toString() + "\n" + state.toString() + "\n" + aiRoot.toString()
  }
}
/* ------------------------------------------------------------
   !ルートGoalAI
   !@memo
------------------------------------------------------------ */
class PGRoot(aOwner:APerson) extends AIGoalComposite[APerson](aOwner)
{
  override def name() = "AIRoot"

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
      if(getOwner.equals(at) == false)
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
      subGoals.head.activate()

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
  override def name() = "PGAction("+action.name+" target "+actionTarget.name+")"
  // override def toString() = name + \n" + super.toString()

  // 
  override def activate()
  {
    super.activate()

    if(isActive() == false)
    {
      addSubGoal(new PGActionRun(getOwner, action, actionTarget))
      addSubGoal(new PGActionMoveTarget(getOwner, action, actionTarget))
      subGoals.head.activate()
      setActive()
    }
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
  override def name() = "PGActionMoveTarget" + "("+action.name+" target "+actionTarget.name+")"

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
  override def name() = "PGActionRun" + "("+action.name+" target "+actionTarget.name+")"

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
/* ------------------------------------------------------------
   !感想Enum
   !@memo
------------------------------------------------------------ */
object Feedback extends Enumeration
{
  val Fun, Sad, Good, NotBad, Bad, TooBad = Value
}
/* ------------------------------------------------------------
 !記憶クラス
 !@memo 見た事、聞いた事、やった事、感じた事などの記憶
 ------------------------------------------------------------ */
class Memory(val time:Float, val place:Vector3, val targetActor:AActor,
	     val targetActor:ActionTarget, val feedback:Feedback.Value, var strength:Float)
{
  
}
