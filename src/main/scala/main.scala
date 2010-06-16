/* ------------------------------------------------------------
 !Sims simple sample
 ------------------------------------------------------------ */

import processing.core._
import scala.util._
import util.tm8st._
  
/* ------------------------------------------------------------
 !ゲーム基本オブジェクト
 !@memo
 ------------------------------------------------------------ */
trait GameObject
{
  var name:String = "GameObject"
  
  private var bDestroy:Boolean = false

  def setDestroy(){ bDestroy = true }
  def isDestroy() = bDestroy
}
/* ------------------------------------------------------------
 !更新可能なもの
 !@memo
 ------------------------------------------------------------ */
trait Tickable
{
  // 
  def tick(delta:Float)
  {
  }
}
/* ------------------------------------------------------------
 !描画可能なもの
 !@memo
 ------------------------------------------------------------ */
trait Drawable extends GameObject
{
  private var drawPriority = 0
  private var bHidden = false
  private var bDrawableResistered = false

  def setDrawPriority(p:Int){ drawPriority = p }
  def getDrawPriority() = drawPriority
  def isHidden() = bHidden  
  def isDrawableResistered() = bDrawableResistered  
  def setHidden(b:Boolean){ bHidden = b }
  def setDrawableResistered(b:Boolean){ bDrawableResistered = b }

  def needsDrawableRegister() = isHidden() == false && isDrawableResistered() == false

  // 
  def draw()
  {
  }
}
/* ------------------------------------------------------------
 !位置を持ち移動するもの
 !@memo
 ------------------------------------------------------------ */
trait Movable extends Tickable
{
  def pos:Vector3
  def bounds:Bounds

  // var pos:Vector3
  // var bounds:Bounds

  //
  def isContain(x:Int, y:Int):Boolean =
  {
    val dx = pos.X - x
    val dy = pos.Y -y
    
    bounds.radius * bounds.radius > dx*dx + dy*dy
  }

  //
  def isIntersect(r:Movable):Boolean =
  {
    val d = (pos - r.pos).size()
    val rad = bounds.radius + r.bounds.radius
    rad*rad > d*d
  }
}
/* ------------------------------------------------------------
 !基本プリミティブ
 !@memo
 ------------------------------------------------------------ */
abstract class Primitive extends GameObject with Movable with Drawable
{
  var bounds = new Bounds(0.f)
  var world:World = null
}
/* ------------------------------------------------------------
 !基本プリミティブ
 !@memo
 ------------------------------------------------------------ */
class CPrimitive extends GameObject with Movable with Drawable
{
  var owner:GameActor = null
  var translation = new Vector3(0.f)
  def pos = if(owner != null) owner.pos + translation else translation
  var bounds = new Bounds(0.f)
  var world:World = null
}
/* ------------------------------------------------------------
 !形状プリミティブ
 !@memo
 ------------------------------------------------------------ */
class CShapePrimitive extends CPrimitive
{
  var strokeColor = new Color(0, 0, 0, 255)
  var fillColor = new Color(0, 0, 0, 255)
  var isFillShape = false

  // 
  override def draw()
  {
    GL.stroke(strokeColor)

    if(isFillShape)
      GL.fill(fillColor)
    else
      GL.noFill()
  }
}
/* ------------------------------------------------------------
 !矩形プリミティブ
 !@memo
 ------------------------------------------------------------ */
class CBoxPrimitive(translation:Vector3, bounds:Bounds) extends CShapePrimitive
{
  // 
  override def draw()
  {
    super.draw()

    GL.rect(pos.X-bounds.boxExtent.X, pos.Y-bounds.boxExtent.Y, bounds.boxExtent.X * 2.f, bounds.boxExtent.Y * 2.f)
  }
}
/* ------------------------------------------------------------
 !球プリミティブ
 !@memo
 ------------------------------------------------------------ */
class CSpherePrimitive(translation:Vector3, bounds:Bounds) extends CShapePrimitive
{
  // 
  override def draw()
  {
    super.draw()
    GL.ellipse(pos.X, pos.Y, bounds.radius, bounds.radius);
  }
}
/* ------------------------------------------------------------
 !ラベルプリミティブ
 !@memo
 ------------------------------------------------------------ */
class CLabelPrimitive(var caption:String, translation:Vector3, bounds:Bounds) extends CShapePrimitive
{
  isFillShape = true

  // 
  override def draw()
  {
    super.draw()
    GL.text(caption, pos.X, pos.Y)
  }
}
/* ------------------------------------------------------------
 !ゲーム用アクタ
 !@memo
 ------------------------------------------------------------ */
trait GameActor extends GameObject with Movable
{
  type Primitives = List[CPrimitive]

  val world:World
  var primitives:Primitives = List()

  // 
  def addPrimitive(p:CPrimitive)
  {
    p.owner = this
    primitives = p::primitives
  }
  // 
  def removePrimitive(p:CPrimitive)
  {
    p.owner = null
    primitives = primitives.filter(_ != p)
  }

  override def setDestroy()
  {
    super.setDestroy()

    primitives.map(_.setDestroy())
  }
  
  //
  override def tick(delta:Float)
  {
    super.tick(delta)

    primitives.map(_.tick(delta))
    primitives.filter(_.needsDrawableRegister()).map(world.addDrawable(_))
  }
}

/* ------------------------------------------------------------
 !アプレット
 !@memo
 ------------------------------------------------------------ */
object SimsApplet extends PApplet
{
  // 
  var game: Game = new Game(1)

  // 
  def main(args: Array[String])
  {
    var frame = new javax.swing.JFrame("Simsましまろ")
    var applet = SimsApplet

    frame.getContentPane().add(applet)
    applet.init()
    frame.pack()
    frame.setVisible(true)
  }

  // 
  override def setup()
  {
    game.setup(this)    
    frameRate(60);
  }

  // 
  override def draw()
  {
    game.draw()

    if(game.isQuit())
      {
	noLoop()
	System.exit(0)
      }
  }

  // 
  override def mouseReleased()
  {
    Logger.debug("mouseReleased: " + mouseX + " " + mouseY + "button " + mouseButton)

    game.mouseReleased(mouseX, mouseY, mouseButton)
  }

  // 
  override def keyPressed()
  {
    Logger.debug("keyPressed: " + key)

    game.keyPressed(key, this)
  }
}
/* ------------------------------------------------------------
 !行動の対象
 !@memo
 ------------------------------------------------------------ */
trait ActionTarget extends GameActor
{
  val actions:List[Action]
}
/* ------------------------------------------------------------
 !オブジェクト
 !@memo
 ------------------------------------------------------------ */
class AObject(name:String, var pos:Vector3, var bounds:Bounds, val world:World, val actions:List[Action]) extends ActionTarget
{
  addPrimitive(new CBoxPrimitive(new Vector3(0.f), bounds))
  val label = new CLabelPrimitive(name, new Vector3(0.f), bounds)
  {
    fillColor = new Color(128, 128, 255)
  }
  addPrimitive(label)
}
/* ------------------------------------------------------------
 !人
 !@memo
 ------------------------------------------------------------ */
class APerson(name:String, var pos:Vector3, val world:World, val actions:List[Action]) extends ActionTarget
{
  var state = PersonState(0.f, 0.f, 0.f, 0.f, 0.f)
  var mode = state.calcMode()
  var actionTarget:ActionTarget = null
  var nextAction:Action = null
  var bounds = new Bounds(12.f)

  val sphere = new CSpherePrimitive(pos, bounds)
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

    if(nextAction == null)
      {
	thinkNextAction()
      }
    else
      {
	val dif = actionTarget.pos - pos;
	Logger.debug("move: " + name + " dif " + dif.toString + " difSize " + dif.size)
	if(dif.size > (bounds.radius + actionTarget.bounds.radius))
	  pos = pos + dif.normal() * 1.f
	else
	  {
	    // pos = actionTarget.pos

	    nextAction.Run(this)
	    world.addEffect(new ASerif(nextAction.name, pos, world))
	    nextAction = null
	    actionTarget = null
	  }
      }

    // State
    state = state.update()
  }

  // 
  def ChangeState(effect:PersonState)
  {
    state = state.affect(effect)
  }
  
  // 
  def thinkNextAction()
  {
    Logger.debug("thinkNextAction: " + name)

    var maxState = state
    var bestAction:Action = null
    var bestActionTarget:ActionTarget = null
    for(at <- world.getActionTargets; a <- at.actions)
      {
	var newState = state.affect(a.effect)
	Logger.debug("mode: new " + newState.calcMode() +", max " + maxState.calcMode() + " if " + (newState.calcMode() > maxState.calcMode()).toString)
	if(newState.calcMode() > maxState.calcMode())
	  {
	    maxState = newState
	    bestAction = a
	    bestActionTarget = at
	  }
      }

    if(bestAction != null)
      {
	nextAction = bestAction
	actionTarget = bestActionTarget

	Logger.debug("Set Action: " + name + " " + nextAction.name)
      }
  }

  // 
  override def toString() =
  {
    var act = if(nextAction!= null) nextAction.name else "null"
    act = " NextAction " + act + "\n"
    "APerson " + name + "\n" + act + " Mode " + state.calcMode().toString() + "\n" + state.toString()
    // "APerson " + name + "\n" + "Mode " + state.calcMode().toString() + "nextAction " + if(nextAction!= null) nextAction.name else "null" + state.toString()
  }
}
/* ------------------------------------------------------------
 !台詞
 !@memo
 ------------------------------------------------------------ */
class ASerif(val caption:String, var pos:Vector3, val world:World) extends GameActor
{
  name = caption
  var bounds = Bounds(new Vector3(14 * caption.length, 12, 0.f), 12.f)
  var timer = 120

  val box = new CBoxPrimitive(pos, bounds)
  {
    strokeColor = new Color(0, 0, 32, 120)
  }
  addPrimitive(box)
  val label = new CLabelPrimitive(caption, pos, bounds)
  {
    strokeColor = new Color(0, 0, 32, 120)
  }
  addPrimitive(label)

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    pos = pos - Vector3(0.0f, 0.5f, 0.f)
    label.strokeColor -= Color(0, 0, 0, 1)
    label.fillColor -= Color(0, 0, 0, 1)
    box.strokeColor -= Color(0, 0, 0, 1)
    timer -= 1
    if(timer < 0)
    {
      setDestroy()
    }
  }
}
/* ------------------------------------------------------------
 !世界
 !@memo
 ------------------------------------------------------------ */
class World(aW:Int, aH:Int)
{
  private val width = aW
  private val height = aH

  var deltaTime = 0.f
  var totalTime = 0.f
  private var persons:List[APerson] = List()
  private var objects:List[AObject] = List()
  private var effects:List[GameActor] = List()
  private var actionTargets:List[ActionTarget] = List()
  private var drawables:List[Drawable] = List()

  def addPerson(person:APerson)
  {
    persons = person :: persons
    actionTargets = person :: actionTargets
  }
  def addObject(aObject:AObject)
  {
    objects = aObject :: objects
    actionTargets = aObject :: actionTargets
  }
  def addEffect(aEffect:GameActor)
  {
    effects = aEffect :: effects
  }
  def addDrawable(aDrawable:Drawable)
  {
    if(aDrawable.needsDrawableRegister())
      {
	aDrawable.setDrawableResistered(true)
	drawables = aDrawable :: drawables
      }
  }

  def getPersons() = persons
  def getObjects() = objects
  def getEffects() = effects
  def getActionTargets() = actionTargets

  def tick(delta:Float)
  {
    deltaTime = delta
    totalTime += delta

    persons.map(_.tick(delta))
    objects.map(_.tick(delta))
    effects.map(_.tick(delta))

    effects = effects.filter(_.isDestroy == false)
  }

  def draw()
  {
    drawables = drawables.filter(_.isDestroy() == false).sort(_.getDrawPriority > _.getDrawPriority)
    drawables.map(_.draw())
  }
}
/* ------------------------------------------------------------
 !人の肉体的、精神的状態
 !@memo
 ------------------------------------------------------------ */
object PersonState
{
  val splineHunger = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBladder = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))
  val splineBoke = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineTsukkomi = new Spline(Array((-1.f, 1.f), (0.f, 1.f), (1.f, 1.f)))
  val splineHp = new Spline(Array((-1.f, 1.f), (0.f, 0.9f), (1.f, 0.3f)))

  def calcHungerWeight(x:Float) = PersonState.splineHunger.map(Util.remap(x, -100.f, 100.f))
  def calcBladderWeight(x:Float) = PersonState.splineBladder.map(Util.remap(x, -100.f, 100.f))
  def calcBokeWeight(x:Float) = PersonState.splineBoke.map(Util.remap(x, -100.f, 100.f))
  def calcTsukkomiWeight(x:Float) = PersonState.splineTsukkomi.map(Util.remap(x, -100.f, 100.f))
  def calcHpWeight(x:Float) = PersonState.splineHp.map(Util.remap(x, -100.f, 100.f))

  def getParamDeclares() = 
  {
    List[(String, Float=>Float)](
      ("お腹", calcHungerWeight),
      ("便意", calcBladderWeight),
      ("ボケ", calcBokeWeight),
      ("ツッコミ", calcTsukkomiWeight),
      ("体力", calcHpWeight),
       )
  }

}
case class PersonState(aHunger:Float, aBladder:Float, aBoke:Float, aTsukkomi:Float, aHp:Float)
{
  val hunger = Util.clamp(aHunger, -100.f, 100.f)
  val bladder = Util.clamp(aBladder, -100.f, 100.f)
  val boke = Util.clamp(aBoke, -100.f, 100.f)
  val tsukkomi = Util.clamp(aTsukkomi, -100.f, 100.f)
  val hp = Util.clamp(aHp, -100.f, 100.f)

  // 
  def affect(r:PersonState):PersonState =
    {
      PersonState(hunger + r.hunger, bladder + r.bladder, boke + r.boke, tsukkomi + r.tsukkomi, hp + r.hp)
    }

  // 
  def update() = 
    {
      PersonState(hunger - 0.001f, bladder - 0.001f, boke - 0.1f, tsukkomi - 0.1f, hp + 0.01f)
      // new PersonState(hunger - 0.05f, bladder - 0.03f, boke - 0.1f, tsukkomi - 0.1f, hp + 0.1f)
    }

  //
  def calcMode():Float =
    {
      PersonState.calcHungerWeight(hunger) * hunger + PersonState.calcBladderWeight(bladder) * bladder + PersonState.calcBokeWeight(boke) * boke + PersonState.calcTsukkomiWeight(tsukkomi) * tsukkomi + PersonState.calcHpWeight(hp) * hp
    }

  //
  override def toString =
  {
    "PersonState:\n hunger " + hunger + "\n bladder " + bladder + "\n boke " + boke + "\n tsukkomi " + tsukkomi + "\n hp " + hp
  }
}
/* ------------------------------------------------------------
 !行動データ
 !@memo
 ------------------------------------------------------------ */
class Action(aName:String, aEffect:PersonState)
{
  val name = aName
  var effect = aEffect

  def Run(aActor:APerson)
  {
    Logger.debug(name)

    aActor.ChangeState(effect)
  }
}
/* ------------------------------------------------------------
 !ゲーム管理
 !@memo
 ------------------------------------------------------------ */
class Game(aNum: Int)
{
  // StateID
  private val GameStop = 0
  private val GamePlay = 1
  private val GameClear = 2
  private val GameOver = 3
  private val GameQuit = 4

  // sizes
  private val WindowSizeX = 800
  private val WindowSizeY = 600
  private val uiFontSize = 12

  private var state = GameStop
  private var uiFont: PFont = new PFont
  private var world: World = null
  private var startTime = 0
  private var lastTime = 0
  private var app:PApplet = null
  private var debugActor:GameActor = null

  // 
  def isQuit(): Boolean = state == GameQuit
  
  // 
  def setup(g: PApplet)
  {
    GL.g = g
    app = g
    app.size(WindowSizeX, WindowSizeY);

    uiFont = app.createFont("SanSerif", uiFontSize)

    app.background(192)

    reset()
  }

  //
  def reset()
  {
    state = GameStop

    world = new World(WindowSizeX, WindowSizeY)

    //define actions
    // class PersonState(aHunger:Float, aBladder:Float, aBoke:Float, aTsukkomi:Float)
    val eat = new Action("食べる", PersonState(30.0f, 0.f, 0.f, 0.f, -10.f))
    val eatSnack = new Action("軽く食べる", PersonState(5.0f, -1.f, 0.f, 0.f, -5.f))
    val drink = new Action("飲む", PersonState(10.0f, -3.f, 0.f, 0.f, -5.f))
    val toilet = new Action("トイレ", PersonState(0.f, 50.f, 0.f, 0.f, -10.f))
    val sit = new Action("座る", PersonState(0.f, 0.f, 0.f, 0.f, 1.f))
    val sleep = new Action("眠る", PersonState(-5.f, -5.f, -5.f, -5.f, 50.f))
    val push = new Action("押す", PersonState(-1.0f, 0.f, 5.f, 0.f, -5.f))
    val wordBoke = new Action("一言ぼけ", PersonState(0.f, 0.f, 20.f, 0.f, -5.f))
    val tsukkomi = new Action("つっこみ", PersonState(0.f, 0.f, 0.f, 20.f, -5.f))

    val bokeActions = List(wordBoke)
    val tsukkomiActions = List(tsukkomi)
    
    //define persons 
    // world.addPerson(new APerson("アナ", new Vector3(128, 128, 0), world))
    world.addPerson(new APerson("美羽", Vector3(64, 128, 0), world, tsukkomiActions))
    world.addPerson(new APerson("千佳", Vector3(128, 32, 0), world, tsukkomiActions))
    // world.addPerson(new APerson("茉莉", new Vector3(198, 198, 0), world))
    // world.addPerson(new APerson("伸恵", new Vector3(256, 320, 0), world))
    
    //define objects
    world.addObject(new AObject("空間", Vector3(320, 320, 0), new Bounds(320.f), world, bokeActions))
    world.addObject(new AObject("ベッド", Vector3(64, 64, 0), new Bounds(64.f), world, List(push, sleep, sit)))
    world.addObject(new AObject("トイレ", Vector3(32, 256, 0), new Bounds(32.f), world, List(push, toilet)))
    world.addObject(new AObject("冷蔵庫", Vector3(320, 128, 0), new Bounds(24.f), world, List(push, eat, drink)))
    world.addObject(new AObject("机", Vector3(196, 128, 0), new Bounds(24.f), world, List(sit, eatSnack, drink)))
    world.addObject(new AObject("勉強机", Vector3(128, 256, 0), new Bounds(16.f), world, List(sit, push)))
  }

  //
  def selectActor(scrX:Int, scrY:Int):GameActor = 
  {
    for(p <- world.getPersons)
      if(p.isContain(scrX, scrY))
	return p
    null
  }
  // 
  def mouseReleased(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    // LEFT, RIGHT, CENTERなどの定数がうまく参照できなかったのでこんな感じ
    val LEFT = 37
    val RIGHT = 39

    if(state != GameQuit)
    {
      if(mouseButton == LEFT)
	debugActor = selectActor(mouseX, mouseY)
    }
  }

  //
  def exit()
  {
    Logger.info("exit.")
    state = GameQuit
  }

  // mouseボタンが押しづらいのでキーボードでも押せるようにする
  def keyPressed(key:Int, g:PApplet)
  {
    if(state == GameStop || state == GamePlay)
      {
	//   key match
	//   {
	// 	case 'a' => open(g.mouseX, g.mouseY)
	// 	case 'f' => frag(g.mouseX, g.mouseY)
	// 	case _ => ()
	//   }
	// }
	if(state != GameQuit)
	  {
	    key match
	    {
	      case 'r' => reset()
	      case 'q' => exit()
	      case _ => ()
	    }
	  }    
      }
  }

  // def bezier(t:Float)(x0:Float, x1:Float, x2:Float:x3:Float):Float =
  // {
  //   x0*(1-t)*(1-t)*(1-t)+3*x1*t*(1-t)*(1-t)*+3*x2t*t*(1-t)+x3*t*t*t
  // }

  // 
  def draw()
  {
    app.background(255)

    // ヘッダ
    app.textFont(uiFont)
    app.stroke(0);
    app.fill(0)
    // GL.text("time " + (world.totalTime/60).toInt + "min.", 32, 32);
    GL.text("time " + world.totalTime + "sec.", 32, WindowSizeY-32);

    // ゲーム世界
    world.tick(1.f / 60.f);
    world.draw();

    // if(debugActor != null)
    // {
    //   debugActor.debugDraw()
    // }

    // 人状態
    {
      
      app.stroke(0);
      app.fill(0)
      var x = 0
      val sx = 32
      val ox = 140
      val sy = 360
      for(p <- world.getPersons())
	{
	  GL.text(p.toString(), sx + x * ox, sy)
	  x += 1
	}
    }

    // 状態の重み関数グラフ
    {
      app.stroke(0);
      app.fill(0)

      val params = PersonState.getParamDeclares()

      val sx = 700.f
      val sy = 200.f
      val oy = 60.f
      val scale = 0.2f
      var y=0
      for(p <- params)
	{
	  app.text(p._1, sx-100 * scale, sy + y * oy-100 * scale)
	  app.stroke(224)
	  GL.line(sx + -100 * scale, sy + y * oy, sx + 100 * scale, sy + y * oy)
	  GL.line(sx + -100 * scale, sy + y * oy - 100*scale, sx + -100 * scale, sy + y * oy + 100*scale)
	  
	  app.stroke(64, 0, 0)
	  for(x <- (-100 to 100).filter(_ % (1/scale).toInt == 0))
	    GL.point(sx + x * scale, sy + (-p._2(x) * x * scale) + y * oy)
	  
	  y += 1
	}
    }
  }
}
