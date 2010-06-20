/* ------------------------------------------------------------
 !Sims simple sample
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
/* ------------------------------------------------------------
 !行動の対象
 !@memo
 ------------------------------------------------------------ */
trait SimsActor extends GameActor
{
  def simsWorld():SimsWorld = world.asInstanceOf[SimsWorld]
}
/* ------------------------------------------------------------
 !行動の対象アクタ
 !@memo
 ------------------------------------------------------------ */
trait ActionTarget extends SimsActor
{
  val actions:List[Action]
}
/* ------------------------------------------------------------
 !オブジェクトアクタ
 !@memo
 ------------------------------------------------------------ */
class AObject(val objectName:String, var pos:Vector3, var bounds:Bounds, val world:World, val actions:List[Action]) extends ActionTarget
{
  override def gameObjectName = "AObject"
  override def name = gameObjectName + ":" + objectName

  addPrimitive(new CBoxPrimitive(new Vector3(0.f), bounds))
  val label = new CLabelPrimitive(objectName, new Vector3(0.f), bounds)
  {
    fillColor = new Color(128, 128, 255)
  }
  addPrimitive(label)
}
/* ------------------------------------------------------------
 !台詞アクタ
 !@memo
 ------------------------------------------------------------ */
class ASerif(val caption:String, var pos:Vector3, val world:World) extends GameActor
{
  override def gameObjectName = "ASerif"
  override def name = gameObjectName + ":" + caption

  var bounds = Bounds(new Vector3(14 * caption.length, 12, 0.f), 12.f)
  var timer = 120

  // primitives
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
class SimsWorld(aW:Int, aH:Int) extends World(aW, aH)
{
  private var persons:List[APerson] = List()
  private var objects:List[AObject] = List()
  private var actionTargets:List[ActionTarget] = List()

  def addPerson(person:APerson)
  {
    persons = person :: persons
    actionTargets = person :: actionTargets
    addActor(person)
  }
  def addObject(aObject:AObject)
  {
    objects = aObject :: objects
    actionTargets = aObject :: actionTargets
    addActor(aObject)
  }

  def getPersons() = persons
  def getObjects() = objects
  def getActionTargets() = actionTargets

  // // 
  // def tick(delta:Float)
  // {
  //   deltaTime = delta
  //   totalTime += delta
  //   actors.map(_.tick(delta))
  //   effects = effects.filter(_.isDestroy == false)
  // }

  // def draw()
  // {
  //   drawables = drawables.filter(_.isDestroy() == false).sort(_.getDrawPriority > _.getDrawPriority)
  //   drawables.map(_.draw())
  // }
}
/* ------------------------------------------------------------
 !シムズゲーム管理
 !@memo
 ------------------------------------------------------------ */
object SimsGame extends Game
{
  override val title = "Simsましまろ"

  // sizes
  override protected val WindowSizeX = 800
  override protected val WindowSizeY = 680
  override protected val uiFontSize = 12

  private var world = new SimsWorld(WindowSizeX, WindowSizeY)
  
  // 
  override def setup(g:PApplet)
  {
    super.setup(g)

    // Logger.currentLevel = Logger.LogDebug
  }

  //
  override def reset()
  {
    state = GameStop

    world = new SimsWorld(WindowSizeX, WindowSizeY)

    //define actions
    // case class PersonState(aHunger:Float, aBladder:Float, aBoke:Float, aTsukkomi:Float, aSocial:Float, aHp:Float)
    // val ChannelBoke = 0x01
    // val ChannelTsukkomi = 0x02
    // val ChannelBisyoujo = 0x04
    // val ChannelMatsuri = 0x08

    val eat = new Action("食べる", PersonState(30.0f, 0.f, 0.f, 0.f, 0.f, -10.f), Action.ChannelUsual)
    val eatSnack = new Action("軽く食べる", PersonState(5.0f, -1.f, 0.f, 0.f, 0.f, -5.f), Action.ChannelUsual)
    val drink = new Action("飲む", PersonState(10.0f, -3.f, 0.f, 0.f, 0.f, -5.f), Action.ChannelUsual)
    val toilet = new Action("トイレ", PersonState(0.f, 50.f, 0.f, 0.f, 0.f, -10.f), Action.ChannelUsual)
    val sit = new Action("座る", PersonState(0.f, 0.f, 0.f, 0.f, 0.f, 1.f), Action.ChannelUsual)
    val sleep = new Action("眠る", PersonState(-5.f, -5.f, -5.f, -5.f, 0.f, 50.f), Action.ChannelUsual)
    val push = new Action("押す", PersonState(-1.0f, 0.f, 5.f, 0.f, 0.f, -5.f), Action.ChannelBoke)
    val wordBoke = new Action("一言ぼけ", PersonState(0.f, 0.f, 20.f, 0.f, 5.f, -5.f), Action.ChannelBoke)
    val tsukkomi = new Action("つっこみ", PersonState(0.f, 0.f, 0.f, 20.f, 5.f, -5.f), Action.ChannelTsukkomi)
    val dakitsuki = new Action("抱きつき", PersonState(0.f, 0.f, 20.f, 0.f, 5.f, -5.f), Action.ChannelOyaji)
    val mitsumeru = new Action("見つめる", PersonState(0.f, 0.f, 20.f, 0.f, 5.f, -5.f), Action.ChannelOyaji)
    val yomu = new Action("読む(絵本)", PersonState(0.f, 0.f, 5.f, 0.f, 0.f, -5.f), Action.ChannelMatsuri)
    val talk = new Action("話す", PersonState(0.f, 0.f, 0.f, 0.f, 10.f, -3.f), Action.ChannelUsual)
    val watchTV = new Action("見る", PersonState(0.f, 0.f, 0.f, 0.f, 1.f, -5.f), Action.ChannelUsual)
    
    //define persons
    val channelAna = Action.ChannelUsual | Action.ChannelTsukkomi
    val channelMiu = Action.ChannelUsual | Action.ChannelTsukkomi | Action.ChannelBoke
    val channelChika = Action.ChannelUsual | Action.ChannelTsukkomi
    val channelMatsuri = Action.ChannelUsual | Action.ChannelMatsuri
    val channelNobue = Action.ChannelUsual | Action.ChannelTsukkomi | Action.ChannelOyaji

    val bokeActions = List(wordBoke, talk)
    val tsukkomiActions = List(tsukkomi, talk)
    val bisyoujoActions = List(dakitsuki, mitsumeru, talk)
    
    // world.addPerson(new APerson("伸恵", new Vector3(256, 320, 0), world, tsukkomiActions, channelNobue))
    // world.addPerson(new APerson("茉莉", new Vector3(198, 198, 0), world, bisyoujoActions, channelMatsuri))
    world.addPerson(new APerson("美羽", Vector3(64, 128, 0), world, tsukkomiActions, channelMiu))
    world.addPerson(new APerson("千佳", Vector3(128, 32, 0), world, tsukkomiActions, channelChika))
    // world.addPerson(new APerson("アナ", new Vector3(128, 128, 0), world, bisyoujoActions, channelAna))
    
    //define objects
    world.addObject(new AObject("空間", Vector3(320, 320, 0), new Bounds(320.f), world, List(wordBoke)))
    world.addObject(new AObject("ベッド", Vector3(64, 64, 0), new Bounds(64.f), world, List(push, sleep, sit)))
    world.addObject(new AObject("トイレ", Vector3(32, 256, 0), new Bounds(32.f), world, List(push, toilet)))
    world.addObject(new AObject("冷蔵庫", Vector3(420, 258, 0), new Bounds(24.f), world, List(push, eat, drink)))
    world.addObject(new AObject("机", Vector3(196, 128, 0), new Bounds(24.f), world, List(sit, eatSnack, drink)))
    world.addObject(new AObject("勉強机", Vector3(128, 256, 0), new Bounds(16.f), world, List(push)))
    world.addObject(new AObject("絵本", Vector3(180, 256, 0), new Bounds(8.f), world, List(yomu)))
    world.addObject(new AObject("テレビ", Vector3(320, 240, 0), new Bounds(12.f), world, List(wordBoke, watchTV)))
  }

  // 
  override def mouseReleased(mouseX:Int, mouseY:Int, mouseButton: Int)
  {
    // LEFT, RIGHT, CENTERなどの定数がうまく参照できなかったのでこんな感じ
    val LEFT = 37
    val RIGHT = 39

    if(state != GameQuit)
    {
      // if(mouseButton == LEFT)
      // 	debugActor = selectActor(mouseX, mouseY)
    }
  }

  //
  override def exit()
  {
    super.exit()
  }

  // mouseボタンが押しづらいのでキーボードでも押せるようにする
  override def keyPressed(key:Int, g:PApplet)
  {
    if(state == GameStop || state == GamePlay)
      {
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

  // 
  override def tick(delta:Float)
  {
    Profiler.auto("Game Tick", "", Color.Black)
    {
      world.tick(delta)
    }
  }
  
  // 
  override def draw()
  {
    Profiler.auto("Game Draw", "", Color.Black)
    {
      app.background(224)

      // ゲーム世界
      world.draw()
      
      // ヘッダ
      app.textFont(uiFont)
      app.stroke(0);
      app.fill(0)
      GL.text("time " + world.totalTime + "sec.", 32, WindowSizeY-32)
      GL.text("processing millis " + Util.getCurrentMSec() + "msec.", 32, WindowSizeY-16)

      // 存在リスト
      {
	world.draw();
	app.stroke(0);
	app.fill(0)
	var x = 0
	val sx = 640
	val sy = 500
	val oy = uiFontSize
	for(a <- world.getActors())
	  {
	    GL.text(a.name, sx, sy + x * oy)
	    x += 1
	  }
      }
      
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
	val sy = 80.f
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

    // プロファイラー描画
    Profiler.draw(32, 32)
  }
  //
  def selectActor(scrX:Int, scrY:Int):GameActor = 
  {
    for(p <- world.getPersons)
      if(p.isContain(scrX, scrY))
	return p
    null
  }
}
/* ------------------------------------------------------------
 !アプレット
 !@memo
 ------------------------------------------------------------ */
object SimsApplet extends PApplet
{
  // 
  var game = SimsGame

  val needFrame = 60.f
  
  // 
  def main(args: Array[String])
  {
    val frame = new javax.swing.JFrame(game.title)
    
    frame.getContentPane().add(this)
    this.init()
    frame.pack()
    frame.setVisible(true)
  }

  // 
  override def setup()
  {
    game.setup(this)
    frameRate(needFrame)
  }

  // 
  override def draw()
  {
    Profiler.beginFrame()

    game.tick(1.f/needFrame)

    game.draw()

    Profiler.endFrame()

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
