import Intcode.{Computer, State}

import scala.io.Source

object Day09 extends App {
  val program = Source.fromResource("Day09Input").mkString

  def run: Unit = {
    val state1 = State(program, 0, List(1))
    val state2 = State(program, 0, List(2))

    val result1 = Computer.run(state1)
    val result2 = Computer.run(state2)

    println(result1.outputs)
    println(result2.outputs)
  }

  run
}
