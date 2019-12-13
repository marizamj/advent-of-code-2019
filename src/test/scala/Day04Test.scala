import org.scalatest.FunSuite

class Day04Test extends FunSuite {
  test("digitsAreNotDecreasing") {
    assert(Day04.digitsAreNotDecreasing(List(1, 2, 3, 4, 5, 6)) === true)
    assert(Day04.digitsAreNotDecreasing(List(1, 1, 1, 1, 1, 1)) === true)
    assert(Day04.digitsAreNotDecreasing(List(2, 4, 6, 8, 6, 6)) === false)
    assert(Day04.digitsAreNotDecreasing(List(2, 2, 3, 4, 5, 0)) === false)
  }

  test("hasPair") {
    assert(Day04.hasPair(List(1, 2, 3, 4, 5, 6)) === false)
    assert(Day04.hasPair(List(1, 1, 1, 1, 1, 1)) === true)
    assert(Day04.hasPair(List(2, 4, 6, 8, 6, 6)) === true)
    assert(Day04.hasPair(List(2, 2, 3, 4, 5, 0)) === true)
  }

  test("hasPair2") {
    assert(Day04.hasPair2(List(1, 2, 3, 4, 5, 6)) === false)
    assert(Day04.hasPair2(List(1, 2, 3, 3, 3, 6)) === false)
    assert(Day04.hasPair2(List(1, 1, 1, 1, 1, 1)) === false)
    assert(Day04.hasPair2(List(1, 1, 1, 1, 2, 2)) === true)
    assert(Day04.hasPair2(List(2, 4, 6, 7, 7, 9)) === true)
    assert(Day04.hasPair2(List(2, 2, 3, 4, 5, 0)) === true)
  }

  test("isValidPassword1") {
    assert(Day04.isValidPassword1(123445) === true)
    assert(Day04.isValidPassword1(111111) === true)
    assert(Day04.isValidPassword1(223450) === false)
    assert(Day04.isValidPassword1(123789) === false)
  }
}
