import org.scalatest.FunSuite

class Day02Test extends FunSuite {
  test("runIntcode") {
    assert(Day02.runIntcode(List(1,0,0,0,99)) === List(2,0,0,0,99))

    assert(Day02.runIntcode(List(2,3,0,3,99)) === List(2,3,0,6,99))

    assert(Day02.runIntcode(List(2,4,4,5,99,0)) === List(2,4,4,5,99,9801))

    assert(Day02.runIntcode(List(1,1,1,4,99,5,6,0,99)) === List(30,1,1,4,2,5,6,0,99))

    assert(Day02.runIntcode(List(1,9,10,3,2,3,11,0,99,30,40,50)) === List(3500,9,10,70,2,3,11,0,99,30,40,50))
  }
}
