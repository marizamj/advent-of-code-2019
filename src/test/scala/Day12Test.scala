import org.scalatest.FunSuite

class Day12Test extends FunSuite {
  test("timeStep") {
    val moons = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))
    val velocity = List.fill(moons.length)((0, 0, 0))

    val nextStep = Day12.timeStep(moons, velocity)

    val expectedMoons = List((2, -1, 1), (3, -7, -4), (1, -7, 5), (2, 2, 0))
    val expectedVelocity = List((3, -1, -1), (1, 3, 3), (-3, 1, -3), (-1, -3, 1))
    assert(nextStep === (expectedMoons, expectedVelocity))
  }

  test("calcTotalEnergy 1") {
    val moons = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))

    assert(Day12.calcTotalEnergy(moons, 10) === 179)
  }

  test("calcTotalEnergy 2") {
    val moons = List((-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3))

    assert(Day12.calcTotalEnergy(moons, 100) === 1940)
  }

  test("calcStepsUntilRepeat 1") {
    val moons = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))

    assert(Day12.calcStepsUntilRepeat(moons) === 2772)
  }

  test("calcStepsUntilRepeat 2") {
    val moons = List((-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3))

    assert(Day12.calcStepsUntilRepeat(moons) === 4686774924L)
  }
}
