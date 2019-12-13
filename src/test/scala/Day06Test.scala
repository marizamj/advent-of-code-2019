import org.scalatest.FunSuite

class Day06Test extends FunSuite {
  test("countOrbits") {
    assert(Day06.countOrbits("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L") === 42)
  }

  test("findShortestDistance") {
    assert(Day06.findShortestDistance("COM)B\nB)C\nC)D\nD)E\nE)F\nB)G\nG)H\nD)I\nE)J\nJ)K\nK)L\nK)YOU\nI)SAN") === 4)
  }
}
