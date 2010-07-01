/* ------------------------------------------------------------
 !gui layout lib.
 ------------------------------------------------------------ */
package tm8st.util.layout

import scala.util._

import tm8st.util._

/* ------------------------------------------------------------
 !レイアウトされる要素オブジェクト
 !@memo
 ------------------------------------------------------------ */
abstract trait LayoutElement
{
  def sizeX:Float
  def sizeY:Float
  def isEnableScaling:Boolean
  def isDrawRect:Boolean
 
  val originalSizeX = sizeX
  val originalSizeY = sizeY
  var backgroundColor = new Color(0, 32, 64, 32)
  
  def drawLayoutElement(x:Float, y:Float)
  {
    GL.stroke(Color.Black)
    GL.fill(backgroundColor)
    GL.rect(x, y, sizeX, sizeY)
  }
}
/* ------------------------------------------------------------
 !文字列要素オブジェクト
 !@memo
 ------------------------------------------------------------ */
class LayoutElementString(val strs:List[String], val font:GLFont, bEnableScaling:Boolean)
  extends LayoutElement
{
  val sizeX = Util.getMaxLineLength(strs) * font.width.toFloat + 10.f
  val sizeY = Util.getLineNum(strs) * font.height.toFloat + 10.f
  def isEnableScaling = bEnableScaling
  def isDrawRect = true

  var textColor = new Color(0, 0, 0, 255)
  
  override def drawLayoutElement(x:Float, y:Float)
  {
    super.drawLayoutElement(x, y)

    GL.stroke(textColor)
    GL.fill(textColor)
    for(s <- strs)
      GL.text(s, x + 1.f, y + 8.f + font.height * strs.indexOf(s), font)
  }
}
/* ------------------------------------------------------------
 !
 !@memo
 ------------------------------------------------------------ */
object GridLayoutManager
{
}
/* ------------------------------------------------------------
 !グリッド方式のレイアウト管理者
 !@memo
 ------------------------------------------------------------ */
class GridLayoutManager(var layoutSizeX:Float, var layoutSizeY:Float, val gridMaxSizeX:Float, val gridMaxSizeY:Float)
{
  type Elements = List[LayoutElement]

  var elements:Elements = List()
  var marginX = 5.f
  var marginY = 5.f
  var sizeMaxX = 0.f
  var sizeMaxY = 0.f

  def addElement(e:LayoutElement)
  {
    elements = e :: elements
  }
  def removeElement(e:LayoutElement)
  {
    elements -= e
  }

  // 
  def drawElements()
  {
    sizeMaxX = Math.max(sizeMaxX, elements.map(_.sizeX).reduceLeft(Math.max(_, _)) + marginX * 2)
    sizeMaxY = Math.max(sizeMaxY, elements.map(_.sizeY).reduceLeft(Math.max(_, _)) + marginY * 2)

    val maxX = Math.min(gridMaxSizeX, sizeMaxX)
    val maxY = Math.min(gridMaxSizeY, sizeMaxY)
    
    Logger.debug("Grid elem " + elements.length)
    Logger.debug("Grid max " + maxX + ":" + maxY)

    val xN = Math.max(1, (layoutSizeX / maxX).toInt - 1)
    val yN = Math.max(1, (layoutSizeY / maxY).toInt - 1)

    Logger.debug("Grid tile " + xN + ":" + yN)

    var id = 0;
    for(y <- 0 to yN; x <- 0 to xN)
    {
      Logger.debug("Grid pos " + x + ":" + y + " id = " + id)

      elements(elements.length - id-1).drawLayoutElement(x*(maxX+marginX) + marginX, y*(maxY+marginY) + marginY)
      
      id += 1
      if(elements.length <= id)
	{
	  elements = List()
	  return
	}
    }

    elements = List()
  } 
}
