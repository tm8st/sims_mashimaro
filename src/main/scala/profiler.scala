/* ------------------------------------------------------------
 !パフォーマンスプロファイラ
 ------------------------------------------------------------ */
package tm8st.util

import scala.util._
import scala.collection.mutable.Stack

import tm8st.util._

/* ------------------------------------------------------------
 !パフォーマンスプロファイラ Singleton
 !@memo return を使っているブロックを渡すとうまくいかない。
 ------------------------------------------------------------ */
object Profiler
{
  private var isActive = true
  private var isBeginFrame = true
  private var nodeStack:Stack[Node] = new Stack
  // 階層表現に使用する文字
  var hierarchySpace = "-"

  // 有効、無効の設定用
  def setActive(flag:Boolean){ isActive = flag }

  // 区間毎の時間計測用クラス
  case class Node(val symbol:String, val caption:String, val color:Color)
  {
    var time = Util.getCurrentNSec()
    var childs:List[Node] = List()

    def addChild(c:Node){ childs = c :: childs }
  }

  // フレーム計測開始通知
  def beginFrame()
  {
    isBeginFrame = true
    nodeStack.clear()
    pushNode(new Node("Root", "", Color.Black))
  }

  // フレーム計測終了通知
  def endFrame()
  {
    // 仕様になった。
    // Debug
    // {
    //   if(nodeStack.length != 1)
    //     Logger.warning("Profiler: invalid nodeStack depth.")
    // }      

    if(isBeginFrame)
    {
      // 計測時間の確定
      while(nodeStack.length != 1)
      {
        popNode()
      }
      nodeStack.top.time = Util.getCurrentNSec() - nodeStack.top.time

      isBeginFrame = false
    }
  }

  // 自動でNodeのpush, popを行うUser用関数
  def auto(symbol:String = "", caption:String = "", c:Color = Color.Black)(block: => Unit)
  {
    if(isActive)
    {
      pushNode(new Node(symbol, caption, c))
      block
      popNode()
    }
    else
    {
      block
    }
  }

  // String形式による計測時間の出力
  def getInfo():List[String] =
  {
    // 状態を確定するためにはすべてのノードの時間が計測済みでなければならないため
    endFrame()

    List("Profiler:") ::: getInfoNode(nodeStack.top, 0)
  }
  // 
  private def getInfoNode(n:Node, depth:Int):List[String] =
  {
    val child:List[String] = n.childs.flatMap(getInfoNode(_, depth + 1))
    List(hierarchySpace * depth + n.symbol + " " + n.caption +": " + n.time / 1000000.0 + "msec") ::: child
  }

  // 記録の数の取得
  def getRecordNum():Int =
  {
    // 状態を確定するためにはすべてのノードの時間が計測済みでなければならないため
    endFrame()

    getRecordNumNode(nodeStack.top)
  }
  private def getRecordNumNode(n:Node):Int =
  {
    if(n == null)
      0
    else
      1 + n.childs.foldLeft(0)(_ + getRecordNumNode(_))
  }

  // 
  private def pushNode(n:Node) =
  {
    if(nodeStack.isEmpty == false)
    {
      Logger.debug("Profilter: addChild " + " "+nodeStack.top.symbol+" to " + n.symbol + " depth " + nodeStack.length)

      nodeStack.top.addChild(n)
    }
    nodeStack.push(n)

    n
  }

  // 
  private def popNode()
  {
    Debug
    {
      if(nodeStack.isEmpty == false)
        Logger.debug("Profilter: popChild " + " "+nodeStack.top.symbol + " depth " + nodeStack.length)
    }      

    nodeStack.top.time = Util.getCurrentNSec() - nodeStack.top.time
    nodeStack.pop()
  }
}
