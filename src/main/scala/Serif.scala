/* ------------------------------------------------------------
 !Sims serif
 ------------------------------------------------------------ */
package tm8st.sims

import tm8st.engine._
import tm8st.util._
import tm8st.sims._
  
/* ------------------------------------------------------------
 !台詞タイプEnum
 !@memo
 ------------------------------------------------------------ */
object SerifType extends Enumeration
{
  val OgoeBoke,
    OgoeTsukkomi,
    Nichijo,
    Empty = Value
}
/* ------------------------------------------------------------
 !デバッグ台詞アクタ
 !@memo
 ------------------------------------------------------------ */
class ADebugSerif(val utterer:SimsActor, val caption:String, var pos:Vector3, val world:World) extends SimsActor
{
  override def gameObjectName = "ADebugSerif"
  override def name = gameObjectName + ":" + caption

  var bounds = Bounds(new Vector3(caption.length * 12 / 2, 12, 0.f), 12.f, pos)
  var timer = 120

  // primitives
  val box = new CBoxPrimitive(Vector3.Zero, bounds)
  {
    strokeColor = new Color(0, 0, 32, 120)
  }
  addPrimitive(box)
  val label = new CLabelPrimitive(caption, Vector3.Zero, bounds, SimsGame.getFont(0))
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
 !台詞アクタ
 !@memo
 ------------------------------------------------------------ */
class ASerif(val utterer:SimsActor, val caption:String, val serifType:SerifType.Value,
             val rangeRadius:Float, val effectTarget:PersonState, var pos:Vector3, val world:World) extends SimsActor
{
  override def gameObjectName = "ASerif"
  override def name = gameObjectName + ":" + caption

  val font = SimsGame.getFont(0)
  var bounds = new Bounds(new Vector3(caption.length / 2.f * font.width, rangeRadius, rangeRadius), rangeRadius, pos)
  val timeMax = 120
  val baseOpacity = 128
  var timer = timeMax
  var heardActors:List[SimsActor] = List(utterer)

  // primitives
  val sphere = new CSpherePrimitive(Vector3.Zero, new Bounds(rangeRadius, pos))
  {
    if(serifType == SerifType.OgoeTsukkomi)
      strokeColor = new Color(128, 0, 0, baseOpacity)
    if(serifType == SerifType.OgoeBoke)
      strokeColor = new Color(0, 0, 128, baseOpacity)
  }
  addPrimitive(sphere)
  // val box = new CBoxPrimitive(Vector3.Zero, bounds)
  // {
  //   strokeColor = new Color(0, 0, 32, 120)
  // }
  // addPrimitive(box)
  val label = new CLabelPrimitive(caption, Vector3.Zero, bounds, font)
  {
    strokeColor = new Color(0, 0, 32, baseOpacity)
  }
  addPrimitive(label)

  // 
  override def tick(delta:Float)
  {
    super.tick(delta)

    // check
    simsWorld().getPersons()
    	.filter(heardActors.contains(_) == false)
    		.filter(_.isIntersect(this))
          .foreach{ a =>
            heardActors = a :: heardActors
            a.changeState(effectTarget)
          }

    // pos = pos - Vector3(0.0f, 0.5f, 0.f)
    label.strokeColor -= Color(0, 0, 0, 1)
    label.fillColor -= Color(0, 0, 0, 1)
    // box.strokeColor -= Color(0, 0, 0, 1)
    sphere.strokeColor -= Color(0, 0, 0, 1)
    val curRadius = rangeRadius * timer / timeMax.toFloat
    bounds = new Bounds(new Vector3(curRadius), curRadius, pos)
    sphere.bounds = bounds

    timer -= 1
    if(timer < 0)
    {
      setDestroy()
    }
  }
}
