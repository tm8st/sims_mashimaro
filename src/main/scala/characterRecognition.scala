/* ------------------------------------------------------------
 !Character Recognition lib.
 ------------------------------------------------------------ */
package tm8st.util.chararec

import scala.util._
import tm8st.util._

/* ------------------------------------------------------------
   !
   !@memo
------------------------------------------------------------ */
object CharacterRecognition
{
  // patterns for learning.
  val SimplePatterns:List[List[Float]] = List(
    List(
      0,0,1,1,1,0,0,  //'0'
      0,1,0,0,0,1,0,
      1,0,0,0,0,0,1,
      1,0,0,0,0,0,1,
      1,0,0,0,0,0,1,
      0,1,0,0,0,1,0,
      0,0,1,1,1,0,0
    ),
    List(
      0,0,0,1,0,0,0,  //'1'
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0
    ),
    List(
      0,0,0,1,1,0,0,  //'2'
      0,0,1,0,0,1,0,
      0,0,1,0,0,1,0,
      0,0,0,0,1,0,0,
      0,0,0,1,0,0,0,
      0,0,1,0,0,0,0,
      0,0,1,1,1,1,0
    ),
    List(
      0,0,0,1,1,0,0,  //'3'
      0,0,1,0,0,1,0,
      0,0,0,0,0,1,0,
      0,0,0,1,1,0,0,
      0,0,0,0,0,1,0,
      0,0,1,0,0,1,0,
      0,0,0,1,1,0,0
    ),
    List(
      0,0,0,0,0,0,1,  //'/'
      0,0,0,0,0,1,0,
      0,0,0,0,1,0,0,
      0,0,0,1,0,0,0,
      0,0,1,0,0,0,0,
      0,1,0,0,0,0,0,
      1,0,0,0,0,0,0
    )
  )
}
/* ------------------------------------------------------------
   !Character Recognition
   !@memo
------------------------------------------------------------ */
class CharacterRecognition
{
  val Patterns = CharacterRecognition.SimplePatterns
  val OutputNum = Patterns.length
  
  // input declare
  val InputWidth = 7
  val InputHeight = 7
  val InputSize = InputWidth * InputHeight

  val HiddenNum = 32

  // learning config
  val OuterCycles = 100
  val InnerCycles = 1000
  val LearningSpeedScale = 1.2f
  val SigmoidSlope = 1.2f

  // 配列の要素の初期化用関数
  private def randomValue(i:Int, j:Int) = Util.fRand() - 0.5f
  private def zeroValue(i:Int, j:Int) = 0.f
  private def randomValue(i:Int) = Util.fRand() - 0.5f
  private def zeroValue(i:Int) = 0.f

  //閾値と重みの初期設定
  var weight_in_hidden = Util.newMultiDimentionArray(randomValue, InputSize, HiddenNum)
  var weight_hidden_out = Util.newMultiDimentionArray(randomValue, OutputNum, HiddenNum)
  var thresh_out = Util.newArray(randomValue, HiddenNum)
  var thresh_hidden = Util.newArray(randomValue, HiddenNum)
  var hidden_out = Util.newArray(zeroValue, HiddenNum)
  var output = Util.newArray(zeroValue, OutputNum)

  // 
  learning()

  def recognition(ptn:Seq[Float]) =
  {
    val output = forwardNeuralNet(ptn.toArray)
    val rec = output.reduceLeft((a, b) => if(a > b) a else b)

    output.indexOf(rec)
  }

  // ネットワークの教育処理
  private def learning()
  {
    //教師信号の設定
    def calcTeachValue(i:Int, j:Int) = if(j == i) 1.f else 0.f
    val teach_array = Util.newMultiDimentionArray(calcTeachValue, OutputNum-1, Patterns.length-1)

    //外部サイクル
    for(outerLoopCnt <- 0 to OuterCycles)
    {
      var outerError = 0.f

      for(ptn <- 0 to Patterns.length-1)
      {
        //パターンに対応した入力と教師信号の設定
        val sample_in = Patterns(ptn).toArray
        val teach = teach_array(ptn).toArray

        for(innerLoopCnt <- 0 to InnerCycles)
        {
          outerError += backwardNeuralNet(sample_in, teach, forwardNeuralNet(sample_in))
        }
      }

      Logger.debug("CharaRec: ["+outerLoopCnt+"]cnt Error = " + outerError)  
    }

    Logger.debug(forwardNeuralNet(Patterns(0).toArray).toList.toString)
   }

   // 順方向演算
   private def forwardNeuralNet(input:Array[Float]) =
   {
     var output = Util.newArray(zeroValue, OutputNum-1)
     var hidden = Util.newArray(zeroValue, HiddenNum-1)

     for(h <- 0 to HiddenNum-1)
     {
       hidden(h) = -thresh_hidden(h)
       for(i <- 0 to InputSize-1)
         hidden(h) += input(i) * weight_in_hidden(h)(i)

       hidden_out(h) = sigmoid(hidden(h))
     }

     var out = Util.newArray(zeroValue, Patterns.length-1)
     
     // 出力層出力の計算
     for(o <- 0 to Patterns.length-1)
     {
       out(o) = -thresh_out(o)
       for(h <- 0 to HiddenNum-1)
         out(o) += hidden_out(h) * weight_hidden_out(h)(o)

       output(o) = sigmoid(out(o))
     }

     output
   }

   // 逆方向演算のメソッド
   private def backwardNeuralNet(sample_in:Array[Float], teach:Array[Float], output:Array[Float]):Float =
   {
     var output_error = Util.newArray(zeroValue, OutputNum-1)
     var hidden_error = Util.newArray(zeroValue, HiddenNum-1)

     // 出力層の誤差の計算
     for(k <- 0 to OutputNum-1)
       output_error(k) = (teach(k)-output(k)) * output(k) * (1.0f-output(k))

     var error = 0.f
     
     // 隠れ層の誤差の計算
     for(h <- 0 to HiddenNum-1)
     {
       var temp_error = 0.f

       for(o <- 0 to OutputNum-1)
         temp_error += output_error(o) * weight_hidden_out(h)(o)

       hidden_error(h) = hidden_out(h) * (1.0f-hidden_out(h)) * temp_error
       error += temp_error
     }
  
     // 重みの補正
     for(o <- 0 to OutputNum-1)
       for(h <- 0 to HiddenNum-1)
         weight_hidden_out(h)(o) += LearningSpeedScale * output_error(o) * hidden_out(h)

     for(h <- 0 to HiddenNum-1)
       for(i <- 0 to InputSize-1)
         weight_in_hidden(h)(i) += LearningSpeedScale * hidden_error(h) * sample_in(i)

     // 閾値の補正
     for(o <- 0 to OutputNum-1)
       thresh_out(o) -= LearningSpeedScale * output_error(o)

     for(h <- 0 to HiddenNum-1)
       thresh_hidden(h) -= LearningSpeedScale * hidden_error(h)

     error
   }

   //Sigmoid関数を計算するメソッド
   private def sigmoid(x:Float) = (1.0f / (1.0f + Math.exp(-SigmoidSlope*x))).toFloat
}
