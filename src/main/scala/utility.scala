/* ------------------------------------------------------------
 !Utility Liblary
 ------------------------------------------------------------ */
package tm8st.util

import scala.util._
import scala.collection.mutable.Stack
import processing.core._

/* ------------------------------------------------------------
 !便利関数まとめ
 !@memo 
 ------------------------------------------------------------ */
object Util
{
  val rand = new Random
  def fRand() = rand.nextFloat()

  def clamp(v:Float, aMin:Float, aMax:Float) = Math.min(Math.max(v, aMin), aMax)
  def clamp(v:Int, aMin:Int, aMax:Int) = Math.min(Math.max(v, aMin), aMax)

  def remap(x:Float, min:Float, max:Float):Float = 
    if(max - min != 0.f) (x - min) /  (max - min) else (x - min)

  def getCurrentMSec() = GL.g.millis()

  def getMaxLineLength(strs:List[String]) =
  {
    if(strs.isEmpty == false)
      strs.reduceLeft((a, b) => if(a.length > b.length) a else b).length
    else
      0
  }
  def getLineNum(strs:List[String]) = strs.length
}
/* ------------------------------------------------------------
 !描画ラッパー
 !@memo 今のところシングルトンになっているだけ。
 ------------------------------------------------------------ */
object GL
{
  var g:PApplet = null

  def fill(c:Color){ g.fill(c.r, c.g, c.b, c.a) }
  def stroke(c:Color){ g.stroke(c.r, c.g, c.b, c.a) }
  def noFill(){ g.noFill() }
  def rect(x:Float, y:Float, w:Float, h:Float){ g.rect(x, y, w, h) }
  def ellipse(x:Float, y:Float, w:Float, h:Float){ g.ellipse(x, y, w, h); }
  def point(x:Float, y:Float){ g.point(x, y) }
  def line(x0:Float, y0:Float, x1:Float, y1:Float){ g.line(x0, y0, x1, y1) }
  def textFont(f:GLFont){ g.textFont(f.handle) }
  def text(s:String, x:Float, y:Float){ g.text(s, x, y) }
  def text(s:String, x:Float, y:Float, font:GLFont)
  {
    g.textFont(font.handle)
    g.text(s, x + font.width/2, y + font.height/2)
  }
}
/* ------------------------------------------------------------
   !フォント
   !@memo
------------------------------------------------------------ */
class GLFont(val size:Int, val faceName:String)
{
  val width = size * 0.62f
  val height = size

  val handle = GL.g.createFont(faceName, height)
}
object Name extends Enumeration
{
  def NameGame = Value
}
/* ------------------------------------------------------------
 !ログ出力管理
 !@memo 
 ------------------------------------------------------------ */
object Logger
{
  // ログレベル定数
  val LogError = 3
  val LogWarning = 2
  val LogInfo = 1
  val LogDebug = 0
  
  // var currentLevel = LogDebug
  var currentLevel = LogInfo

  // 各種ログ出力
  def info(msg: => String)
  {
    if(LogInfo >= currentLevel)
      log("Info", msg)
  }
  def debug(msg: => String)
  {
    if(LogDebug >= currentLevel)
      log("Debug", msg)
  }
  def warning(msg: => String)
  {
    if(LogWarning >= currentLevel)
      log("Warning", msg)
  }
  def error(msg: => String)
  {
    if(LogError >= currentLevel)
      log("Error", msg)
  }

  private def log(prefix:String, msg:String)
  {
    println("<" + prefix + ">" + "[%5.5f".format(Util.getCurrentMSec()/1000.f) + "]" + msg)
  }
}
/* ------------------------------------------------------------
 !色型
 !@memo
 ------------------------------------------------------------ */
object Color
{
  val Black = Color(0, 0, 0, 255)
  val White = Color(255, 255, 255, 255)
  val Red = Color(255, 0, 0, 255)
  val Green = Color(0, 255, 0, 255)
  val Blue = Color(0, 0, 255, 255)
}
/* ------------------------------------------------------------
 !色型
 !@memo
 ------------------------------------------------------------ */
case class Color(ar:Int, ag:Int, ab:Int, aa:Int)
{
  def this(v:Int) = this(v, v, v, v)
  def this(ar:Int, ab:Int, ag:Int) = this(ar, ab, ag, 255)

  def +(o:Color):Color = Color(r+o.r, g+o.g, b+o.b, a+o.a)
  def -(o:Color):Color = Color(r-o.r, g-o.g, b-o.b, a-o.a)

  val r = Util.clamp(ar, 0, 255)
  val g = Util.clamp(ag, 0, 255)
  val b = Util.clamp(ab, 0, 255)
  val a = Util.clamp(aa, 0, 255)
}
/* ------------------------------------------------------------
 !3要素ベクトル
 !@memo
 ------------------------------------------------------------ */
object Vector3
{
  val Zero:Vector3 = Vector3(0.f, 0.f, 0.f)
}
case class Vector3(aX:Float, aY:Float, aZ:Float)
{
  val X = aX
  val Y = aY
  val Z = aZ

  def this(v:Float) = this(v, v, v)  

  def +(r:Vector3) = new Vector3(X+r.X, Y+r.Y, Z+r.Z)
  def -(r:Vector3) = new Vector3(X-r.X, Y-r.Y, Z-r.Z)
  
  def *(r:Float) = new Vector3(X*r, Y*r, Z*r)

  //
  def size() = java.lang.Math.sqrt(X*X + Y*Y + Z*Z).toFloat

  //
  def normal():Vector3 =
  {
    val len = size()
    if(len == 0.f)
      new Vector3(0.f, 0.f, 0.f)
    else
      {
	val iLen = 1.f / len
	new Vector3(X*iLen, Y*iLen, Z*iLen)
      }
  }

  //
  override def toString() =
  {
    "Vector3(" + X + ", " + Y + ", " + Z + ")"
  }  
}
/* ------------------------------------------------------------
 !衝突判定用型
 !@memo
 ------------------------------------------------------------ */
case class Bounds(val boxExtent:Vector3, val radius:Float)
{
  def this(r:Float) = this(Vector3(r,r,r), r)
  
  override def toString() =
  {
      "Bounds boxExtent " + boxExtent + " radius" + radius
  }  
}
/* ------------------------------------------------------------
   !スプライン補間
   !@memo http://sakura.bb-west.ne.jp/spr/damayan/algo/spline.html
------------------------------------------------------------ */
class Spline(points:Array[(Float, Float)])
{
  val x:Array[Float] = for(p <- points) yield p._1
  val y:Array[Float] = for(p <- points) yield p._2
  require(x.length == y.length)

  def N() = x.length
  var z = new Array[Float](N)
  makeTable()

  def makeTable() = 
  {
    val h = new Array[Float](N)
    val d = new Array[Float](N)

    // 両端点での y''(x) / 6 ... (自然スプライン)
    z(0) = 0; z(N - 1) = 0;
    for(i <- 0 to N-2)
      {
	h(i) =  x(i + 1) - x(i);
	d(i + 1) = (y(i + 1) - y(i)) / h(i);
      }

    z(1) = d(2) - d(1) - h(0) * z(0);
    d(1) = 2 * (x(2) - x(0));
    for(i <- 1 to N-3)
      {
	val t = h(i) / d(i);
	z(i + 1) = d(i + 2) - d(i + 1) - z(i) * t;
	d(i + 1) = 2 * (x(i + 2) - x(i)) - h(i) * t;
      }
    z(N - 2) -= h(N - 2) * z(N - 1);
    for(i <- N-2 to 0)
      {
	z(i) = (z(i) - h(i) * z(i + 1)) / d(i);
      }
  }

  def map(t:Float):Float =
  {
    var i = 0
    var j = N - 1;
    while (i < j) {
      val k = (i + j) / 2;
      if (x(k) < t) i = k + 1;  else j = k;
    }
    if (i > 0) i -= 1;
    val h = x(i + 1) - x(i)
    val d = t - x(i)
    return (((z(i + 1) - z(i)) * d / h + z(i) * 3) * d
	    + ((y(i + 1) - y(i)) / h
	       - (z(i) * 2 + z(i + 1)) * h)) * d + y(i);
  }
}
