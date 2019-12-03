package day_01

import org.scalatest.FunSuite

class SolutionTest extends FunSuite {
  test("calcFuelRequired") {
    assert(Solution.calcFuelRequired(12) === 2)

    assert(Solution.calcFuelRequired(14) === 2)

    assert(Solution.calcFuelRequired(1969) === 654)

    assert(Solution.calcFuelRequired(100756) === 33583)
  }

  test("calcFuelRequiredForFuel") {
    assert(Solution.calcFuelRequiredForFuel(0) === 0)

    assert(Solution.calcFuelRequiredForFuel(2) === 0)

    assert(Solution.calcFuelRequiredForFuel(654) === 312)

    assert(Solution.calcFuelRequiredForFuel(33583) === 16763)
  }
}
