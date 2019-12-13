import org.scalatest.FunSuite

class Day08Test extends FunSuite {
  test("decodeImage") {
    assert(Day08.decodeImage("0222112222120000", 2, 2) === "0110")
  }
}
