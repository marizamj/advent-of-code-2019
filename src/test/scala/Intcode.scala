import Intcode.{Computer, State}
import org.scalatest.FunSuite

import scala.io.Source

class IntcodeTest extends FunSuite {
  test("parseProgram") {
    assert(State.parseProgram("1,2,3,4,5,6") === Map(0 -> 1, 1 -> 2, 2 -> 3, 3 -> 4, 4 -> 5, 5 -> 6))
  }

  test("Using position mode, consider whether the input is equal to 8") {
    val state1 = State("3,9,8,9,10,9,4,9,99,-1,8", 0, List(8))
    val state2 = State("3,9,8,9,10,9,4,9,99,-1,8", 0, List(10))

    val resultTrue = Computer.run(state1)
    val resultFalse = Computer.run(state2)

    assert(resultTrue.outputs === List(1))
    assert(resultFalse.outputs === List(0))
  }

  test("Using position mode, consider whether the input is less than 8") {
    val state1 = State("3,9,7,9,10,9,4,9,99,-1,8", 0, List(6))
    val state2 = State("3,9,7,9,10,9,4,9,99,-1,8", 0, List(10))

    val resultTrue = Computer.run(state1)
    val resultFalse = Computer.run(state2)

    assert(resultTrue.outputs === List(1))
    assert(resultFalse.outputs === List(0))
  }

  test("Using immediate mode, consider whether the input is equal to 8") {
    val state1 = State("3,3,1108,-1,8,3,4,3,99", 0, List(8))
    val state2 = State("3,3,1108,-1,8,3,4,3,99", 0, List(0))

    val resultTrue = Computer.run(state1)
    val resultFalse = Computer.run(state2)

    assert(resultTrue.outputs === List(1))
    assert(resultFalse.outputs === List(0))
  }

  test("Using immediate mode, consider whether the input is less than 8") {
    val state1 = State("3,3,1107,-1,8,3,4,3,99", 0, List(3))
    val state2 = State("3,3,1107,-1,8,3,4,3,99", 0, List(8))

    val resultTrue = Computer.run(state1)
    val resultFalse = Computer.run(state2)

    assert(resultTrue.outputs === List(1))
    assert(resultFalse.outputs === List(0))
  }

  test("Jump test using position mode:" +
    "output 0 if the input was zero or 1 if the input was non-zero") {
    val state1 = State("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 0, List(5))
    val state2 = State("3,12,6,12,15,1,13,14,13,4,13,99,-1,0,1,9", 0, List(0))

    val resultNonZero = Computer.run(state1)
    val resultZero = Computer.run(state2)

    assert(resultNonZero.outputs === List(1))
    assert(resultZero.outputs === List(0))
  }

  test("Jump test using immediate mode:" +
    "output 0 if the input was zero or 1 if the input was non-zero") {
    val state1 = State("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 0, List(567))
    val state2 = State("3,3,1105,-1,9,1101,0,0,12,4,12,99,1", 0, List(0))

    val resultNonZero = Computer.run(state1)
    val resultZero = Computer.run(state2)

    assert(resultNonZero.outputs === List(1))
    assert(resultZero.outputs === List(0))
  }

  test("Output 999 if the input value is below 8, " +
    "output 1000 if the input value is equal to 8, " +
    "or output 1001 if the input value is greater than 8") {

    val program = "3,21,1008,21,8,20,1005,20,22,107,8,21,20,1006,20,31,1106,0,36,98,0,0,1002,21,125,20,4,20,1105,1,46,104,999,1105,1,46,1101,1000,1,20,4,20,1105,1,46,98,99"

    val state1 = State(program, 0, List(0))
    val state2 = State(program, 0, List(8))
    val state3 = State(program, 0, List(3476572))

    val resultBelow = Computer.run(state1)
    val resultEqual = Computer.run(state2)
    val resultGreater = Computer.run(state3)

    assert(resultBelow.outputs === List(999))
    assert(resultEqual.outputs === List(1000))
    assert(resultGreater.outputs === List(1001))
  }

  test("Takes no input and produces a copy of itself as output") {
    val state = State("109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99")

    val result = Computer.run(state)

    assert(result.outputs === List(109,1,204,-1,1001,100,1,100,1008,100,16,101,1006,101,0,99))
  }

  test("Should output a 16-digit number") {
    val state = State("1102,34915192,34915192,7,4,7,99,0")

    val result = Computer.run(state)

    assert(result.outputs.head.toString.length === 16)
  }

  test("Should output the large number in the middle") {
    val state = State("104,1125899906842624,99")

    val result = Computer.run(state)

    assert(result.outputs === List(1125899906842624L))
  }

  test("My input program from day 9") {
    val program = Source.fromResource("Day09Input").mkString
    val state1 = State(program, 0, List(1))
    val state2 = State(program, 0, List(2))

    val result1 = Computer.run(state1)
    val result2 = Computer.run(state2)

    assert(result1.outputs === List(2351176124L))
    assert(result2.outputs === List(73110))
  }

  test("Somebody else's program from day 9") {
    val program = Source.fromResource("test/Day09TestInput1").mkString
    val state1 = State(program, 0, List(1))
    val state2 = State(program, 0, List(2))

    val result1 = Computer.run(state1)
    val result2 = Computer.run(state2)

    assert(result1.outputs === List(3839402290L))
    assert(result2.outputs === List(35734))
  }
}
