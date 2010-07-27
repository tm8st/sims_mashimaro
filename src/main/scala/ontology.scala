/* ------------------------------------------------------------
 !オントロジー用クラス群
 ------------------------------------------------------------ */
package tm8st.util.ontlogy

import scala.util._
import scala.collection._

import tm8st.util._

/* ------------------------------------------------------------
   !動詞の種類
   !@memo
------------------------------------------------------------ */
object VerbType extends Enumeration
{
  val Intransitive, // 自動詞
      Transitive,		// 他動詞
      DoubleTransitive = Value  // 二重他動詞
}
/* ------------------------------------------------------------
   !動詞クラス
   !@memo
------------------------------------------------------------ */
case class Verb(name:String, verbType:VerbType.Value)
  
/* ------------------------------------------------------------
   !基底概念
   !@memo
------------------------------------------------------------ */
class ONTBase
{
  def name = "基底"
  // 名詞
  def noun = "なし"
  // 動詞
  def verbs:List[Verb] = List()
  // 抽象性
  def abstractness = 1.f
}

// フィクション系
class ONTFiction extends ONTBase
{
  override def name = "フィクション"
  override def abstractness = 0.9f
}
class ONTManga extends ONTFiction
{
  override def name = "マンガ"
  override def abstractness = 0.8f
}
class ONTDoraemon extends ONTManga
{
  override def name = "ドラえもん"
  override def abstractness = 0.5f
}

// 行動系
class ONTAction extends ONTBase
{
  override def name = "行動"
  override def abstractness = 1.f 
}
class ONTHave extends ONTAction
{
  override def name = "所有"
  override def verbs = List(Verb("もつ", VerbType.Transitive))
  override def abstractness = 0.5f
}
class ONTChange extends ONTAction
{
  override def name = "変身"
  override def verbs = List(Verb("なる", VerbType.Transitive))
  override def abstractness = 0.5f
}

/* ------------------------------------------------------------
   !オントロジーの操作まとめ用クラス
   !@memo
------------------------------------------------------------ */
object OntlogyManager
{
  def getOntlogies() =
  {
    List[ONTBase](new ONTFiction,
                  new ONTManga,
                  new ONTDoraemon,
                  new ONTAction,
                  new ONTHave,
                  new ONTChange)
  }
}
