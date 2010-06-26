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
  private var isActive = true
  private var isBeginFrame = true
  private var nodeStack:Stack[Node] = new Stack
  var hierarchySpace = "-"

  def setActive(flag:Boolean){ isActive = flag }

  // 区間毎の時間計測用
  case class Node(val symbol:String, val caption:String, val color:Color)
  {
    var time = Util.getCurrentMSec()
    var childs:List[Node] = List()

    def addChild(c:Node){ childs = c :: childs }
  }
  
  // 自動でNodeのpush, popを行うUser向けの便利関数
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
  def pushNode(n:Node):Node =
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
  def beginFrame()
  {
    isBeginFrame = true
    nodeStack.clear()
    pushNode(new Node("Root", "", Color.Black))
  }

  //
  def endFrame()
  {
    assert(nodeStack.length == 1)

    if(isBeginFrame)
      nodeStack.top.time = Util.getCurrentMSec() - nodeStack.top.time

    isBeginFrame = false
  }

  // String形式による計測時間の出力
  def getInfo():List[String] =
  {
    // 状態を確定するためにはすべてのノードの時間が計測済みでなければならないため
    endFrame();

    List("Profiler:") ::: getInfoNode(nodeStack.top, 0)
  }
  // 
  private def getInfoNode(n:Node, depth:Int):List[String] =
  {
    val child:List[String] = n.childs.flatMap(getInfoNode(_, depth + 1))
    List(hierarchySpace * depth + n.symbol + " " + n.caption +": " + n.time + "msec") ::: child
  }
}
