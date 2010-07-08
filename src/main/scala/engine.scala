/* ------------------------------------------------------------
 !simple game engine lib.
 ------------------------------------------------------------ */
package tm8st.engine

import processing.core._
import scala.util._
import tm8st.util._

/* ------------------------------------------------------------
 !ゲーム基本オブジェクト
 !@memo
 ------------------------------------------------------------ */
trait GameObject
{
  def gameObjectName = "GameObject"
  def name = gameObjectName

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
 !移動するもの
 !@memo
 ------------------------------------------------------------ */
trait Movable extends Tickable
{
  def pos:Vector3
  def bounds:Bounds

  def move(dif:Vector3)

  // def move(dif:Vector3)
  // {
  //   pos = pos + dif
  //   bounds = new Bounds(bounds.boxExtent, bounds.radius, pos)
  // }
 
  //
  def isContain(x:Int, y:Int):Boolean = bounds.isContain(x, y)
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
  var bounds = new Bounds(1.f, pos)
  var world:World = null
}
/* ------------------------------------------------------------
 !プリミティブコンポーネント
 !@memo
 ------------------------------------------------------------ */
abstract class CPrimitive extends GameObject with Movable with Drawable
{
  var owner:GameActor = null
  var translation:Vector3
  def pos = if(owner != null) owner.pos + translation else translation
  var bounds = new Bounds(0.f, pos)
  var world:World = null

  def move(dif:Vector3)
  {
    translation = translation + dif
    bounds = new Bounds(bounds, pos)
  }
}
/* ------------------------------------------------------------
 !形状プリミティブ
 !@memo
 ------------------------------------------------------------ */
abstract class CShapePrimitive extends CPrimitive
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
class CBoxPrimitive(var translation:Vector3, bounds:Bounds) extends CShapePrimitive
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
class CSpherePrimitive(var translation:Vector3, bounds:Bounds) extends CShapePrimitive
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
class CLabelPrimitive(var caption:String, var translation:Vector3, bounds:Bounds, var font:GLFont) extends CShapePrimitive
{
  isFillShape = true
  var isCentering = true

  // 
  override def draw()
  {
    super.draw()
    val centeringOffset = if(isCentering) bounds.boxExtent.X else 0.f
    GL.text(caption, pos.X - centeringOffset, pos.Y, font)
  }
}
/* ------------------------------------------------------------
 !ゲーム用アクタ
 !@memo
 ------------------------------------------------------------ */
trait GameActor extends GameObject with Movable
{
  val world:World

  type Primitives = List[CPrimitive]

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
 !世界
 !@memo
 ------------------------------------------------------------ */
class World(aW:Int, aH:Int)
{
  private val width = aW
  private val height = aH

  var deltaTime = 0.f
  var totalTime = 0.f
  def currentTime = totalTime

  private var actors:List[GameActor] = List()
  private var drawables:List[Drawable] = List()

  def getActors() = actors
  def addActor(aActor:GameActor)
  {
    actors = aActor :: actors
  }
  def getDrawables() = drawables
  def addDrawable(aDrawable:Drawable)
  {
    if(aDrawable.needsDrawableRegister())
    {
	    aDrawable.setDrawableResistered(true)
	    drawables = aDrawable :: drawables
    }
  }

  // 
  def tick(delta:Float)
  {
    Profiler.auto("World Tick", "", Color.Black)
    {
      deltaTime = delta
      totalTime += delta

      actors.map(_.tick(delta))

      actors = actors.filter(_.isDestroy == false)
    }
  }

  // 
  def draw()
  {
    drawables = drawables.filter(_.isDestroy() == false)
    drawables = drawables.sortWith(_.getDrawPriority > _.getDrawPriority)
    drawables.map(_.draw())
  }
}
/* ------------------------------------------------------------
 !ゲーム管理
 !@memo
 ------------------------------------------------------------ */
abstract class Game
{
  val title = "Unknwon"
  
  // StateID
  protected val GameStop = 0
  protected val GamePlay = 1
  protected val GameClear = 2
  protected val GameOver = 3
  protected val GameQuit = 4

  // sizes
  protected val WindowSizeX = 800
  protected val WindowSizeY = 800
  
  protected val uiFontSize = 12
  protected val uiFontName = "Osaka"
  protected var uiFont:GLFont = null

  protected var state = GameStop
  protected var startTime = 0
  protected var lastTime = 0
  protected var app:PApplet = null

  // 
  def isQuit(): Boolean = state == GameQuit
  
  // 
  def setup(g: PApplet)
  {
    GL.g = g
    app = g
    app.size(WindowSizeX, WindowSizeY);

    uiFont = new GLFont(uiFontSize, uiFontName)

    reset()
  }

  def mouseReleased(mouseX:Int, mouseY:Int, mouseButton: Int)
  def keyPressed(key:Int, g:PApplet)

  //
  def reset()

  //
  def exit()
  {
    Logger.info("request exit.")
    state = GameQuit
  }

  // 
  def draw()

  // 
  def tick(delta:Float)
}
/* ------------------------------------------------------------
 !自分用アプレット
 !@memo
 ------------------------------------------------------------ */
abstract class MyApplet extends PApplet
{
  // 
  def game:Game
  val needFrame = 60.f
  
  // 
  def main(args: Array[String])
  {
    val frame = new javax.swing.JFrame(game.title)
    
    frame.getContentPane().add(this)
    this.init()
    frame.pack()
    frame.setVisible(true)

    Logger.info("initialized Applet ")
  }

  // 
  override def setup()
  {
    game.setup(this)
    frameRate(needFrame)

    Logger.info("need frame rate " + needFrame)
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
  // 
  override def draw()
  {
    Profiler.beginFrame()

    game.tick(1.f/needFrame)
    game.draw()

    Profiler.endFrame()

    if(game.isQuit())
    {
      Logger.info("exit")
      
      noLoop()
      System.exit(0)
    }
  }
}
