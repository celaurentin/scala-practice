
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

class CollectionTest extends AnyFlatSpec with Matchers {


  /////////////////////////////////////////////////////////////////////////////

  "Range" should "be the difference between the largest and smallest numbers" in {
    range(3, 17, 15, 11, 9) should be(14)
    range(-100, 0, 12, 33) should be(133)
    range(-10, -2) should be(8)
  }

  def range(numbers: Int*): Int = {
    val largest  = numbers.max
    val smallest = numbers.min
    largest - smallest
  }

  /////////////////////////////////////////////////////////////////////////////

  "Mean" should "be the average of the numbers" in {
    mean(13, 19, null, 14, 16, 5, 8) should be(12.5)
  }

  def mean(numbers: Integer*): Double = {
    val filteredNumbers = numbers.filterNot(_==null)
    filteredNumbers.foldLeft(0.0)(_ + _) / filteredNumbers.length
  }

  /////////////////////////////////////////////////////////////////////////////

  "Median" should "be the middle number in a sorted list when there is only one middle number" in {
    median(7, 11, 6, 2, 5) should be(6)
    median(7, 10, 3) should be(7)
  }
  "Median" should "be the average of the middle numbers in a sorted list when there are two middle numbers" in {
    median(13, 18, 14, 16, 5, 8) should be(13.5)
  }

  def median(numbers: Int*): Double = {
    val length = numbers.length
    val sortedNumbers = numbers.sorted
    sortedNumbers.length % 2 match {
      case 0 => mean(sortedNumbers(length / 2 - 1), sortedNumbers(length / 2))
      case 1 => sortedNumbers(length / 2)
    }
  }

  /////////////////////////////////////////////////////////////////////////////

  "Mode" should "be the most commonly occurring number(s) in a list" in {
    mode(5, 2, 3, 6, 4, 1, 3) should be(Array(3))
    mode(4, 5, 3, 1, 3, 2, 5, 6) should be(Array(3, 5))
    mode(4, 5, 5, 3, 1, 3, 2, 5, 6) should be(Array(5))
    mode(1, 3, 2, 4, 5) should be(Array(1, 2, 3, 4, 5))
  }

  def mode(numbers: Int*): Array[Int] = {

    val numbersMap = numbers.groupBy(identity).view.mapValues(_.size).toMap
    val maxValue   = numbersMap.values.max
    numbersMap.collect {
      case (k, v) if v == maxValue => k
    }.toArray
     .sorted
  }

  /////////////////////////////////////////////////////////////////////////////

  "FindSum" should "be the sum of all even elements in a list" in {
    findSum(5, 2, 3, 6, 4, 1, 3) should be(12)
    findSum(-1, 0, 3, 20) should be(20)
    findSum(0, 0, 0) should be(0)
  }

  def findSum(numbers: Int*): Int = numbers.filter(_ % 2 ==0).sum

  /////////////////////////////////////////////////////////////////////////////

  "CountStrings" should "be the count of the number of strings that start with a specific character" in {
    countStrings("c", "abjks", "bzoiu32", "c453", "cotpoi9", "ar5t33") should be(2)
    countStrings("z", "bfsdfs", "c09usfd", "c3joips", "kporr") should be(0)
  }

  def countStrings(startChar: String, words: String*): Int = words.count(_.startsWith(startChar))

  /////////////////////////////////////////////////////////////////////////////
  "CountWords" should "be the count of words that have length greater than 5" in {
    countWords("apple", "banana", "cherry", "coconut", "apple") should be(3)
    countWords("blue", "yellow", "green", "white") should be(1)
  }

  def countWords(words: String*): Int = words.count(_.length>5)

  /////////////////////////////////////////////////////////////////////////////
  "SquareRoot" should "be a list with the square of each number in the input list" in {
    squareRoot(1, 2, 3, 4, 5) should be(List(1, 4, 9, 16, 25))
  }

  def squareRoot(numbers: Int*): List[Int] = numbers.map(n => n*n).toList

  /////////////////////////////////////////////////////////////////////////////
  "Concatenate" should "concatenate all the strings in a list into a single string" in {
    concatenate("1", "2", "3", "4", "5") should be("12345")
  }

  def concatenate(words: String*): String = words.foldLeft("")(_ + _)

  /////////////////////////////////////////////////////////////////////////////
  "SortedUpperCase" should "convert each word to uppercase and sort them in alphabetical order" in {
    toUpperSorted("zulu", "charlie", "alfa", "delta", "beta") should be(List("ALFA","BETA","CHARLIE","DELTA","ZULU"))
  }

  def toUpperSorted(words: String*): Seq[String] = words.map(_.toUpperCase).sorted
}
