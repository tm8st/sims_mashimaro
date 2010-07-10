
import sbt._ 
 
class SimsProject(info: ProjectInfo) extends DefaultProject(info)
{
  val scalaToolsSnapshots = ScalaToolsSnapshots
  val scalatest = "org.scalatest" % "scalatest" % "1.0.1-for-scala-2.8.0.RC1-SNAPSHOT"
  val proguard = "net.sf.proguard" % "proguard" % "4.4"
  val processing = "org.processing" % "core" % "1.1"
}
