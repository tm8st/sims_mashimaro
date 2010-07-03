import org.scalatest.FunSuite
import tm8st.util._
import tm8st.util.chararec._

class TestSuite extends FunSuite
{
  test("""Util clamp""")
  {
    assert(Util.clamp(-1.f, 0.f, 1.f) == 0.f)
    assert(Util.clamp(3.f, 0.f, 1.f) == 1.f)
  }

  test("""Util remap""")
  {
    assert(Util.remap(3.f, 3.f, 6.f) == 0.f)
    assert(Util.remap(6.f, 3.f, 6.f) == 1.f)
    assert(Util.remap(4.5f, 3.f, 6.f) == 0.5f)
  }

  test("""Util line""")
  {
    assert(Util.getMaxLineLength(List("a", "bb", "ccc", "dddd", "eeeee")) == 5)
    assert(Util.getMaxLineLength(List("a", "bb", "ccc", "dddd", "eeeee")) != 0)

    assert(Util.getLineNum(List("a", "bb", "ccc", "dddd", "eeeee")) == 5)
  }

  test("""Util newArray""")
  {
    def calcValue(i:Int) = i
    val arr = Util.newArray(calcValue, 3)
    assert(arr.toString equals "Array(0, 1, 2, 3)")
    // assert(arr == Array(0, 1, 2, 3))
  }

  test("""CharaRec""")
  {
    val cr = new CharacterRecognition()

    assert(cr.recognition(CharacterRecognition.SimplePatterns(0)) == 0)
    assert(cr.recognition(CharacterRecognition.SimplePatterns(1)) == 1)
  }
}
