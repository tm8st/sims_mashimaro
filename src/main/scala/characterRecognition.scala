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
class CharacterRecognition
{
   // int X0=10,X1=125;
   // int Y0=55,Y1=70,Y2=160,Y3=240,Y4=305;

   // int RX0=30,RX1=60,RX2=210,RX3=260;
   // int RY0=225,RY1=240;

  // input declare
  val InputWidth = 7
  val InputHeight = 7
  val InputSize = InputWidth * InputHeight
  
  // int[] sample_in=new int[INPUT];               //学習用入力
  // int[] written_in=new int[INPUT];              //認識用手書き入力

  // int[] teach=new int[PATTERN];            //教師信号

  val HiddenNum = 16
  val OuterCycles = 100
  val InnerCycles = 1000
  val LearningSpeedScale = 1.2f
  val SigmoidSlope = 1.2f

  // val weight_ih_hidden:List[Float] = List()
  // val thresh_hidden:List[Float] = List()
  // val weight_hidden_out:List[Float] = List()
  // val thresh_out:List[Float] = List()
  // val result_out:List[Float] = List()

  //学習用入力データの基となるパターン
  val Patterns:List[List[Float]] = List(
    List(
      0,0,1,1,1,0,0,  //'0'
      0,1,0,0,0,1,0,
      1,0,0,0,0,0,1,
      1,0,0,0,0,0,1,
      1,0,0,0,0,0,1,
      1,0,0,0,0,0,1,
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
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0
    ),
    List(
      0,0,0,0,0,0,1,  //'/'
      0,0,0,0,0,1,0,
      0,0,0,0,0,1,0,
      0,0,0,0,1,0,0,
      0,0,0,0,1,0,0,
      0,0,0,1,0,0,0,
      0,0,0,1,0,0,0,
      0,0,1,0,0,0,0,
      0,0,1,0,0,0,0,
      0,1,0,0,0,0,0,
      1,0,0,0,0,0,0
    )
  )
  
  //パターンと出力すべき教師信号の比較表
  //教師信号の設定
  val teach_array = for(p <- 0 to Patterns.length-1) yield
    	                  for(o <- 0 to Patterns.length-1) yield if(p == o) 1.f else 0.f
  //閾値と重みの初期設定
  var thresh_hidden:Array[Float] = (for(i <- 0 to HiddenNum) yield Util.fRand() - 0.5f).toArray
  var weight_in_hidden:Array[Array[Float]] = (for(h <- 0 to HiddenNum) yield
                                      (for(i <- 0 to InputSize) yield Util.fRand() - 0.5f).toArray).toArray
  var thresh_out:Array[Float] = (for(i <- 0 to HiddenNum) yield Util.fRand() - 0.5f).toArray
  var weight_hidden_out:Array[Array[Float]] = (for(h <- 0 to HiddenNum) yield
                            (for(i <- 0 to Patterns.length) yield Util.fRand() - 0.5f).toArray).toArray

  var hidden_out:Array[Float] = (for(i <- 0 to HiddenNum-1) yield 0.f).toArray
  var output:Array[Float] = (for(i <- 0 to Patterns.length-1) yield 0.f).toArray
  
  println(teach_array)
  println(thresh_hidden)
  println(weight_in_hidden)
  println(thresh_out)
  println(weight_hidden_out)

  //----------------------- 学習 -----------------------
  //外部サイクル
  // for(p=0;p<OUTER_CYCLES;p++)
  for(outerLoopCnt <- 0 to OuterCycles)
  {
    //外部二乗誤差
    var outerError = 0.f

    //パターンの切り替え
    // for(q=0;q<PATTERN;q++)
    for(ptn <- 0 to Patterns.length-1)
    {
      //パターンに対応した入力と教師信号の設定
      val sample_in = Patterns(ptn).toArray
      val teach = teach_array(ptn).toArray

      //内部サイクル
      // for(r=0;r<INNER_CYCLES;r++)
      for(innerLoopCnt <- 0 to InnerCycles)
      {
        //順方向演算
        val output = forwardNeuralNet(sample_in)

        //逆方向演算（バックプロパゲーション）
        outerError += backwardNeuralNet(sample_in, teach, output)
      }

      //内部二乗誤差の計算
      // var inner_error = 0.0f
      // // for(k=0;k<OUTPUT;k++)
      // for(o <- 0 to Patterns.length-1)
      //   inner_error += (teach(o)-output(o)) * (teach(o)-output(o))

      // outer_error += inner_error   //外部二乗誤差への累加算
    }

    println("CharaRec: ["+outerLoopCnt+"]cnt Error = " + outerError)
      
    //外部サイクルの回数と外部二乗誤差の表示
    // g.drawString("実行中の外部サイクルの回数と二乗誤差",X0,Y2);
    // g.setColor(new Color(255,255,192));
    // g.fillRect(X0+5,Y2+10,200,50);   //以前の表示を消去
    // g.setColor(Color.black);
    // g.drawString("OuterCycles="+String.valueOf(p),X0+10,Y2+25);
    // g.drawString("TotalSquaredError=" +String.valueOf(outer_error),X0+10,Y2+45);
  }

  println(teach_array)
  println(thresh_hidden)
  println(weight_in_hidden)
  println(thresh_out)
  println(weight_hidden_out)

  println(forwardNeuralNet(Patterns(0).toArray).toList)
    
   //順方向演算のメソッド
   def forwardNeuralNet(input:Array[Float]) =
   {
     var output = (for(t <- 0 to Patterns.length-1) yield 0.f).toArray
  
      // 隠れ層出力の計算
     var hidden = (for(t <- 0 to HiddenNum-1) yield 0.f).toArray
     //for(int j=0;j<HIDDEN;j++)
     for(h <- 0 to HiddenNum-1)
     {
       hidden(h) = -thresh_hidden(h)
       // for(int i=0;i<INPUT;i++)
       for(i <- 0 to InputSize-1)
         hidden(h) += input(i) * weight_in_hidden(h)(i)

       hidden_out(h) = sigmoid(hidden(h))
     }

     var out = (for(t <- 0 to Patterns.length-1) yield 0.f).toArray
     
     // 出力層出力の計算
     // for(int k=0;k<OUTPUT;k++)
     for(o <- 0 to Patterns.length-1)
     {
       out(o) = -thresh_out(o)
       // for(int j=0;j<HIDDEN;j++)
       for(h <- 0 to HiddenNum-1)
         out(o) += hidden_out(h) * weight_hidden_out(h)(o)

       output(o) = sigmoid(out(o))
     }

     output
   }

   //逆方向演算のメソッド
   def backwardNeuralNet(sample_in:Array[Float], teach:Array[Float], output:Array[Float]):Float =
   {
     var output_error = (for(t <- 0 to Patterns.length-1) yield 0.f).toArray
     var hidden_error = (for(t <- 0 to HiddenNum-1) yield 0.f).toArray

     // 出力層の誤差の計算
     // for(k=0;k<OUTPUT;k++)
     for(k <- 0 to Patterns.length-1)
       output_error(k) = (teach(k)-output(k)) * output(k) * (1.0f-output(k))

     var error = 0.f
     
     // 隠れ層の誤差の計算
     // for(j=0;j<HIDDEN;j++)
     for(h <- 0 to HiddenNum-1)
     {
       var temp_error = 0.f
       // for(k=0;k<OUTPUT;k++)
       for(o <- 0 to Patterns.length-1)
         temp_error += output_error(o) * weight_hidden_out(h)(o)

       hidden_error(h) = hidden_out(h) * (1.0f-hidden_out(h)) * temp_error
       error += temp_error
     }
  
     // 重みの補正
     // for(k=0;k<OUTPUT;k++)
     //   for(j=0;j<HIDDEN;j++)
     for(o <- 0 to Patterns.length-1)
       for(h <- 0 to HiddenNum-1)
         weight_hidden_out(h)(o) += LearningSpeedScale * output_error(o) * hidden_out(h)
     // for(j=0;j<HIDDEN;j++)
     //   for(i=0;i<INPUT;i++)
     for(h <- 0 to HiddenNum-1)
       for(i <- 0 to InputSize-1)
         weight_in_hidden(h)(i) += LearningSpeedScale * hidden_error(h) * sample_in(i)

     // 閾値の補正
     // for(k=0;k<OUTPUT;k++)
     for(o <- 0 to Patterns.length-1)
       thresh_out(o) -= LearningSpeedScale * output_error(o)
     // for(j=0;j<HIDDEN;j++)
     for(h <- 0 to HiddenNum-1)
       thresh_hidden(h) -= LearningSpeedScale * hidden_error(h)

     error
   }

   //Sigmoid関数を計算するメソッド
   def sigmoid(x:Float) = (1.0f / (1.0f + Math.exp(-SigmoidSlope*x))).toFloat
         
//------------------ 学習結果の確認 ------------------

  // g.drawString("学習結果の確認",X0,Y3);
  // for(k=0;k<OUTPUT;k++){
  //   g.drawString("Output",X1+45*k,Y3+25);
  //   g.drawString("  ["+String.valueOf(k)+"]",X1+5+45*k,Y3+40);
  // }

  // for(q=0;q<PATTERN;q++){

//             //入力パターンの設定
//             sample_in=sample_array[q];

//             //順方向演算
//             forwardNeuralNet(sample_in,recog_out);

//             //結果の表示
//             g.setColor(Color.black);
//             g.drawString("TestPattern["+String.valueOf(q)+"]",
//                X0+10,Y4+20*q);
//             for(k=0;k<OUTPUT;k++){
//                if(recog_out[k]>0.99){      //99% より大は赤で YES
//                   g.setColor(Color.red);
//                   string="YES";
//                }
//                else if(recog_out[k]<0.01){ // 1% より小は青で NO
//                   g.setColor(Color.blue);
//                   string="NO ";
//                }
//                else{                       // 1% 以上 99% 以下は黒で?
//                   g.setColor(Color.black);
//                   string=" ? ";
//                }
//                g.drawString(string,X1+10+45*k,Y4+20*q);
//             }

//          }
//       }

}
//       //--------------------------------------------------------
//       //---------------------- 学習モード ----------------------
//       //--------------------------------------------------------
//       if(learning_flag){

//          //閾値と重みの乱数設定
//          for(j=0;j<HIDDEN;j++){
//             thresh_h[j]=(float)Math.random()-0.5f;
//             for(i=0;i<INPUT;i++)
//                weight_ih[i][j]=(float)Math.random()-0.5f;
//          }
//          for(k=0;k<OUTPUT;k++){
//             thresh_o[k]=(float)Math.random()-0.5f;
//             for(j=0;j<HIDDEN;j++)
//                weight_ho[j][k]=(float)Math.random()-0.5f;
//          }

//          //----------------------- 学習 -----------------------

//          for(p=0;p<OUTER_CYCLES;p++){     //外部サイクル

//             outer_error=0.0f;         //外部二乗誤差のクリヤー

//             for(q=0;q<PATTERN;q++){   //パターンの切り替え

//                //パターンに対応した入力と教師信号の設定
//                sample_in=sample_array[q];
//                teach=teach_array[q];

//                for(r=0;r<INNER_CYCLES;r++){   //内部サイクル

//                   //順方向演算
//                   forwardNeuralNet(sample_in,recog_out);

//                   //逆方向演算（バックプロパゲーション）
//                   backwardNeuralNet();

//                }

//                //内部二乗誤差の計算
//                inner_error=0.0f;   //内部二乗誤差のクリヤー
//                for(k=0;k<OUTPUT;k++)
//                   inner_error+=(teach[k]-recog_out[k])
//                      *(teach[k]-recog_out[k]);

//                outer_error+=inner_error;   //外部二乗誤差への累加算

//             }

//             //外部サイクルの回数と外部二乗誤差の表示
//             g.drawString("実行中の外部サイクルの回数と二乗誤差",X0,Y2);
//             g.setColor(new Color(255,255,192));
//             g.fillRect(X0+5,Y2+10,200,50);   //以前の表示を消去
//             g.setColor(Color.black);
//             g.drawString("OuterCycles="+String.valueOf(p),X0+10,Y2+25);
//             g.drawString("TotalSquaredError="
//                +String.valueOf(outer_error),X0+10,Y2+45);

//          }

//          //------------------ 学習結果の確認 ------------------

//          g.drawString("学習結果の確認",X0,Y3);
//          for(k=0;k<OUTPUT;k++){
//             g.drawString("Output",X1+45*k,Y3+25);
//             g.drawString("  ["+String.valueOf(k)+"]",X1+5+45*k,Y3+40);
//          }

//          for(q=0;q<PATTERN;q++){

//             //入力パターンの設定
//             sample_in=sample_array[q];

//             //順方向演算
//             forwardNeuralNet(sample_in,recog_out);

//             //結果の表示
//             g.setColor(Color.black);
//             g.drawString("TestPattern["+String.valueOf(q)+"]",
//                X0+10,Y4+20*q);
//             for(k=0;k<OUTPUT;k++){
//                if(recog_out[k]>0.99){      //99% より大は赤で YES
//                   g.setColor(Color.red);
//                   string="YES";
//                }
//                else if(recog_out[k]<0.01){ // 1% より小は青で NO
//                   g.setColor(Color.blue);
//                   string="NO ";
//                }
//                else{                       // 1% 以上 99% 以下は黒で?
//                   g.setColor(Color.black);
//                   string=" ? ";
//                }
//                g.drawString(string,X1+10+45*k,Y4+20*q);
//             }

//          }
//       }

//       //--------------------------------------------------------
//       //---------------------- 認識モード ----------------------
//       //--------------------------------------------------------
//       else{
//          g.setColor(Color.black);
//          g.drawString("マウスで数字を描いてください",RX0,RY0);
//          g.drawRect(RX1-1,RY1-1,WIDTH*10+2,HEIGHT*10+2);    //外枠
//          g.setColor(Color.gray);
//          for(j=1;j<HEIGHT;j++)
//             g.drawLine(RX1,RY1+10*j,RX1+WIDTH*10,RY1+10*j);
//          for(i=1;i<WIDTH;i++)
//             g.drawLine(RX1+10*i,RY1,RX1+10*i,RY1+HEIGHT*10);
//          for(i=0;i<INPUT;i++)
//             written_in[i]=0;     //手書き入力データのクリヤ
//       }

//    }

//    //順方向演算のメソッド
//    public void forwardNeuralNet(int[] input, float[] output){

//       float[] out=new float[OUTPUT];
//       float[] hidden=new float[HIDDEN];

//       //隠れ層出力の計算
//       for(int j=0;j<HIDDEN;j++){
//          hidden[j]=-thresh_h[j];
//          for(int i=0;i<INPUT;i++)
//             hidden[j]+=input[i]*weight_ih[i][j];
//             hidden_out[j]=sigmoid(hidden[j]);
//       }

//       //出力層出力の計算
//       for(int k=0;k<OUTPUT;k++){
//          out[k]=-thresh_o[k];
//          for(int j=0;j<HIDDEN;j++)
//             out[k]+=hidden_out[j]*weight_ho[j][k];
//             output[k]=sigmoid(out[k]);
//       }

//    }

//    //逆方向演算のメソッド
//    public void backwardNeuralNet(){

//       int i,j,k;

//       float[] output_error=new float[OUTPUT];  //出力層の誤差
//       float[] hidden_error=new float[HIDDEN];  //隠れ層の誤差

//       float temp_error;

//       //出力層の誤差の計算
//       for(k=0;k<OUTPUT;k++)
//          output_error[k]=(teach[k]-recog_out[k])
//             *recog_out[k]*(1.0f-recog_out[k]);

//       //隠れ層の誤差の計算
//       for(j=0;j<HIDDEN;j++){
//          temp_error=0.0f;
//          for(k=0;k<OUTPUT;k++)
//             temp_error+=output_error[k]*weight_ho[j][k];
//             hidden_error[j]=hidden_out[j]
//                *(1.0f-hidden_out[j])*temp_error;
//       }

//       //重みの補正
//       for(k=0;k<OUTPUT;k++)
//          for(j=0;j<HIDDEN;j++)
//             weight_ho[j][k]+=ALPHA*output_error[k]*hidden_out[j];
//       for(j=0;j<HIDDEN;j++)
//          for(i=0;i<INPUT;i++)
//             weight_ih[i][j]+=ALPHA*hidden_error[j]*sample_in[i];

//       //閾値の補正
//       for(k=0;k<OUTPUT;k++)
//          thresh_o[k]-=ALPHA*output_error[k];
//       for(j=0;j<HIDDEN;j++)
//          thresh_h[j]-=ALPHA*hidden_error[j];

//    }

//    //Sigmoid関数を計算するメソッド
//    public float sigmoid(float x){

//       return 1.0f/(1.0f+(float)Math.exp(-BETA*x));

//    }

//    //入力文字を認識するメソッド
//    public void recognizeCharacter(){

//       Graphics g=getGraphics();
//       String string;

//       //順方向演算
//       forwardNeuralNet(written_in,recog_out);

//       //結果の表示
//       for(int k=0;k<OUTPUT;k++){
//           g.setColor(Color.black);
//           g.drawString(String.valueOf(k)+"である",RX2,RY1+20*k);
//           if(recog_out[k]>0.8f)  g.setColor(Color.red);
//           else                   g.setColor(Color.black);

//           g.fillRect(RX3,RY1-10+20*k,(int)(200*recog_out[k]),10);
//           g.drawString(String.valueOf((int)(100*recog_out[k]+0.5f))
//              +"%",RX3+(int)(200*recog_out[k])+10,RY1+20*k);
//        }

//    }
// }
