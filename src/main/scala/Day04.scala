import scala.annotation.tailrec

object Day04 extends App {
  val input = (235741, 706948)

  @tailrec
  def digitsAreNotDecreasing(digits: List[Int]): Boolean = {
    if (digits.tail.isEmpty) true
    else digits.head <= digits.tail.head && digitsAreNotDecreasing(digits.tail)
  }

  @tailrec
  def hasPair(digits: List[Int]): Boolean = {
    if (digits.length == 1) false
    else if (digits.head == digits(1)) true
    else hasPair(digits.tail)
  }

  def hasPair2(digits: List[Int]): Boolean = {
    for (i <- 0 to 9) {
      if (digits.count(_ == i) == 2) return true
    }
    false
  }

  def isValidPassword1(n: Int): Boolean = {
    val digits: List[Int] = n.toString.map(_.asDigit).toList
    digitsAreNotDecreasing(digits) && hasPair(digits)
  }

  def isValidPassword2(n: Int): Boolean = {
    val digits: List[Int] = n.toString.map(_.asDigit).toList
    digitsAreNotDecreasing(digits) && hasPair2(digits)
  }

  def findMatchingPasswords(current: Int, last: Int): Int = {
    var counter = 0

    for (n <- current to last) {
      if (isValidPassword2(n)) counter += 1
    }

    counter
  }

  def run: Unit = {
    val answer = findMatchingPasswords(input._1, input._2)

    println(answer)
  }

  run
}
