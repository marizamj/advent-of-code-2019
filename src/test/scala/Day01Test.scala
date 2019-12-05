import org.scalatest.FunSuite

class Day01Test extends FunSuite {
  test("calcFuelRequired") {
    assert(Day01.calcFuelRequired(12) === 2)

    assert(Day01.calcFuelRequired(14) === 2)

    assert(Day01.calcFuelRequired(1969) === 654)

    assert(Day01.calcFuelRequired(100756) === 33583)
  }

  test("calcFuelRequiredForFuel") {
    assert(Day01.calcFuelRequiredForFuel(0) === 0)

    assert(Day01.calcFuelRequiredForFuel(2) === 0)

    assert(Day01.calcFuelRequiredForFuel(654) === 312)

    assert(Day01.calcFuelRequiredForFuel(33583) === 16763)
  }
}
