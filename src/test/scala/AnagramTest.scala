import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class AnagramTest  extends AnyFlatSpec with Matchers {

  "isAnagram" should "be true between the two words" in {
    isAnagram("silent", "listen") should be(true)
  }

  "isAnagram" should "be false between the two words" in {
    isAnagram("house", "car") should be(false)
    isAnagram("car", "kar") should be(false)
  }

  def isAnagram(word1: String, word2: String): Boolean = {
    if (word1.length!=word2.length) false
    else
      word1.sorted == word2.sorted
  }


}
