/* ------------------------------------------------------------
 !パフォーマンスプロファイラ
 ------------------------------------------------------------ */
package tm8st.util

import scala.util._
import scala.collection.mutable.Stack
import tm8st.util._

/* ------------------------------------------------------------
 !パフォーマンスプロファイラ Singleton
 !@memo 
 ------------------------------------------------------------ */
object Profiler
{
  var isActive = true
  var isBeginFrame = true
  var nodeStack:Stack[Node] = new Stack

  //
  case class Node(val symbol:String, val caption:String, val color:Color)
  {
    var time = Util.getCurrentMSec()
    var childs:List[Node] = List()

    def addChild(c:Node){ childs = c :: childs }
  }

  // 
  def pushNode(n:Node) =
  {
    if(nodeStack.isEmpty == false)
    {
      Logger.debug("ProfilterNode: addChild " + " "+nodeStack.top.symbol+" to " + n.symbol)

      nodeStack.top.addChild(n)
    }
    nodeStack.push(n)

    return n
  }
  // 
  def popNode()
  {
    nodeStack.pop()
  }

  // 
  def auto(symbol:String, caption:String, c:Color)(block: => Unit)
  {
    if(isActive)
    {
      var n = pushNode(new Node(symbol, caption, c))
      block
      n.time = Util.getCurrentMSec() - n.time
      popNode()
    }
    else
    {
      block
    }
  }

  //
  def beginFrame()
  {
    isBeginFrame = true
    nodeStack.clear()
    pushNode(new Node("Root", "", Color.Black))
  }

  //
  def endFrame()
  {
    isBeginFrame = false
    assert(nodeStack.length == 1)
    if(nodeStack.top.time < 999.f)
      nodeStack.top.time = Util.getCurrentMSec() - nodeStack.top.time
  }

  //
  def getInfo():List[String] =
  {
    // 状態を確定するためにはすべてのノードの時間が計測済みでなければならないため
    assert(nodeStack.length == 1)
    
    if(isBeginFrame)
      {
	nodeStack.top.time = Util.getCurrentMSec() - nodeStack.top.time
      }
    List("Profiler:") ::: getInfoNode(nodeStack.top, 0)
  }
  //
  private def getInfoNode(n:Node, depth:Int):List[String] =
  {
    val space = "-"

    val child:List[String] = n.childs.flatMap(getInfoNode(_, depth + 1))
    List(space * depth + n.symbol + " " + n.caption +": " + n.time + "msec") ::: child
  } 
  def draw(x:Int, y:Int)
  {
    // 描画するためにはすべてのノードの時間が計測済みでなければならないため
    assert(nodeStack.length == 1)
  
    if(isBeginFrame)
    {
      nodeStack.top.time = Util.getCurrentMSec() - nodeStack.top.time
    }

    GL.stroke(Color.Black)
    GL.fill(Color.Black)

    GL.text("Profiler:", x, y)
    drawNode(nodeStack.top, x, y + 16, 0)
  }
  //
  private def drawNode(n:Node, x:Int, y:Int, depth:Int)
  {
    // Logger.debug("Profilter: drawNode "+n.name+" childNum " + n.childs.length)
 
    GL.stroke(n.color)
    GL.fill(n.color)

    val space = "-"
    
    GL.text(space * depth + n.symbol + " " + n.caption +": " + n.time + "msec", x, y)
    
    for(c <- n.childs)
    {
      drawNode(c, x, y + 14 * (1 + n.childs.indexOf(c)), depth +1)
    }
  }
}
