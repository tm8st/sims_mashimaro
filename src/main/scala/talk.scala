/* ------------------------------------------------------------
 !会話用クラス群
 ------------------------------------------------------------ */
package tm8st.util.talk

import scala.util._
import scala.collection._

import tm8st.util._
import tm8st.util.ontlogy._


/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
class TalkDatabase
{
  def initialize()
  {
    generateBokes()
  }
  
  // オントロジーデータを元にボケデータを作成
  private def generateBokes()
  {
    val ontlogies = OntlogyManager.getOntlogies()
    
  }
}
