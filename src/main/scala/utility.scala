/* ------------------------------------------------------------
 !Utility Liblary
 ------------------------------------------------------------ */

package util.tm8st
import processing.core._

/* ------------------------------------------------------------
 !Utility
 !@memo 
 ------------------------------------------------------------ */
object Util
{
  def clamp(v:Float, aMin:Float, aMax:Float) = Math.min(Math.max(v, aMin), aMax)
  def clamp(v:Int, aMin:Int, aMax:Int) = Math.min(Math.max(v, aMin), aMax)
}
/* ------------------------------------------------------------
 !GL
 !@memo 
 ------------------------------------------------------------ */
object GL
{
  var g:PApplet = null

  def fill(c:Color){ g.fill(c.r, c.g, c.b, c.a) }
  def stroke(c:Color){ g.stroke(c.r, c.g, c.b, c.a) }
  def noFill(){ g.noFill() }
  def rect(x:Float, y:Float, w:Float, h:Float){ g.rect(x, y, w, h) }
  def ellipse(x:Float, y:Float, w:Float, h:Float){ g.ellipse(x, y, w, h); }
  def text(s:String, x:Float, y:Float){ g.text(s, x, y) }
  def point(x:Float, y:Float){ g.point(x, y) }
  def line(x0:Float, y0:Float, x1:Float, y1:Float){ g.line(x0, y0, x1, y1) }
}
/* ------------------------------------------------------------
 !Logger
 !@memo 
 ------------------------------------------------------------ */
object Logger
{
  val LogError = 3
  val LogWarning = 2
  val LogInfo = 1
  val LogDebug = 0
  var currentLevel = LogWarning

  def info(msg: => String)
  {
    if(LogInfo >= currentLevel)
      println(msg)
  }
  def debug(msg: => String)
  {
    if(LogDebug >= currentLevel)
      println(msg)
  }
  def warning(msg: => String)
  {
    if(LogWarning >= currentLevel)
      println(msg)
  }
  def error(msg: => String)
  {
    if(LogError >= currentLevel)
      println(msg)
  }
}
/* ------------------------------------------------------------
 !
 !@memo
 ------------------------------------------------------------ */
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

  override def toString() =
    {
      "Vector3(" + X + ", " + Y + ", " + Z + ")"
    }  
}
/* ------------------------------------------------------------
 !
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
 !色設定用
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
   !スプライン補間(１次元)
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
/*
 f(-5.5)=0.0293056736081598
 f(-5.0)=0.038461538461538464
 f(-4.5)=0.04761740331491712
 f(-4.0)=0.058823529411764705
 f(-3.5)=0.07468172670683233
 f(-3.0)=0.1
 f(-2.5)=0.1400810292242694
 f(-2.0)=0.2
 f(-1.5)=0.29734709757256067
 f(-1.0)=0.5
 f(-0.5)=0.8205305804854879
 f(0.0)=1.0
 f(0.5)=0.8205305804854879
 f(1.0)=0.5
 f(1.5)=0.2973470975725607
 f(2.0)=0.20000000000000007
 f(2.5)=0.1400810292242694
 f(3.0)=0.10000000000000002
 f(3.5)=0.07468172670683233
 f(4.0)=0.058823529411764705
 f(4.5)=0.04761740331491712
 f(5.0)=0.038461538461538464
 f(5.5)=0.029305673608159794
 */
