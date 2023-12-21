import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should._

import scala.collection.convert.ImplicitConversions.`collection asJava`

class ExerciseTest extends AnyFlatSpec with Matchers {

  /////////////////////////////////////////////////////////////////////////////


  "Range" should "be the difference between the largest and smallest numbers" in {
    range(3, 17, 15, 11, 9) should be(14)
  }

  def range(numbers: Int*): Int = {
    val largest = numbers.max
    val smallest = numbers.min
    largest - smallest
  }


  /////////////////////////////////////////////////////////////////////////////


  "Mean" should "be the average of the numbers" in {
    mean(13, 19, null, 14, 16, 5, 8) should be(12.5)
  }

  def mean(numbers: Integer*): Double = {
    val filteredNumbers = numbers.filter(x => x!=null)
    numbers.foldLeft(0.0)(_ + _) / filteredNumbers.length
  }


  /////////////////////////////////////////////////////////////////////////////


  "Median" should "be the middle number in a sorted list when there is only one middle number" in {
    median(7, 11, 6, 2, 5) should be(6)
  }
  "Median" should "be the average of the middle numbers in a sorted list when there are two middle numbers" in {
    median(13, 18, 14, 16, 5, 8) should be(13.5)
  }

  def median(numbers: Int*): Double = {
    val length = numbers.length
    val sortedNumbers = numbers.sorted
    sortedNumbers.length % 2 match {
        case 0 => mean(sortedNumbers(length/2 -1),sortedNumbers(length/2))
        case 1 => mean(sortedNumbers(length/2))
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
    val maxValue = numbersMap.values.max
    val maxKeys = numbersMap.collect{case (k,v) if v==maxValue => k}

    numbersMap.collect {
      case (k,v) if maxKeys.contains(k) => k
    }.toArray.sorted
  }


  /////////////////////////////////////////////////////////////////////////////

}
