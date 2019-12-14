import org.scalatest.FunSuite

class Day11Test extends FunSuite {
  test("getNewDirection: current UP, turn left") {
    assert(Day11.getNewDirection(0, 0) === 3)
  }

  test("getNewDirection: current UP, turn right") {
    assert(Day11.getNewDirection(0, 1) === 1)
  }

  test("getNewDirection: current RIGHT, turn left") {
    assert(Day11.getNewDirection(1, 0) === 0)
  }

  test("getNewDirection: current RIGHT, turn right") {
    assert(Day11.getNewDirection(1, 1) === 2)
  }

  test("getNewDirection: current DOWN, turn left") {
    assert(Day11.getNewDirection(2, 0) === 1)
  }

  test("getNewDirection: current DOWN, turn right") {
    assert(Day11.getNewDirection(2, 1) === 3)
  }

  test("getNewDirection: current LEFT, turn left") {
    assert(Day11.getNewDirection(3, 0) === 2)
  }

  test("getNewDirection: current LEFT, turn right") {
    assert(Day11.getNewDirection(3, 1) === 0)
  }

  test("getNewPosition: (0, 0) go up") {
    assert(Day11.getNewPosition((0, 0), 0) === (0, -1))
  }

  test("getNewPosition: (0, 0) go right") {
    assert(Day11.getNewPosition((0, 0), 1) === (1, 0))
  }

  test("getNewPosition: (0, 0) go down") {
    assert(Day11.getNewPosition((0, 0), 2) === (0, 1))
  }

  test("getNewPosition: (0, 0) go left") {
    assert(Day11.getNewPosition((0, 0), 3) === (-1, 0))
  }
}
