/* ------------------------------------------------------------
 !Logger File.
 ------------------------------------------------------------ */
package tm8st.util

import scala.collection.mutable.ListBuffer
import scala.util._

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

  var logs = new ListBuffer[String]()
  
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
    val logmsg = "<" + prefix + ">" + "[%5.5f".format(Util.getCurrentMSec()/1000.f) + "]" + msg
    println(logmsg)

    logs += logmsg
  }

  def outputFile()
  {
    ArchiveOutput.save("log.txt", logs.foldLeft("Logs:\n")(_ + _ + "\n"))
  }
}
