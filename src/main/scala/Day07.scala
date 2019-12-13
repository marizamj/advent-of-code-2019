import Intcode.{Computer, State}

import scala.io.Source

object Day07 extends App {
  val program = Source.fromResource("Day07Input").mkString

  def findMaxSignal1(program: String): Long = {
    var allOutputs: List[Long] = List()

    for (permutation <- List(0, 1, 2, 3, 4).permutations) {
      var output: List[Long] = List(0)
      for (input <- permutation) {
        val state = State(program, 0, input :: output)
        val result = Computer.run(state)
        output = result.outputs
      }
      allOutputs = allOutputs.concat(output)
    }

    allOutputs.max
  }

  def updateAmplifiers(amplifiers: List[State]): List[State] = {
    val withNewInputs = amplifiers.zipWithIndex.map(ampWithIndex => {
      val (amp, i) = ampWithIndex
      val targetAmp = if (i < amplifiers.length - 1) amplifiers(i + 1) else amplifiers.head
      amp.updateInputs(amp.inputs.concat(targetAmp.outputs))
    })
    withNewInputs.map(_.updateOutputs(Nil))
  }

  def findMaxSignal2(program: String): Long = {
    var allOutputs: List[Long] = Nil

    for (permutation <- List(5, 6, 7, 8, 9).permutations) {
      var amplifiers = permutation.zipWithIndex.map(input => {
        val inputs = if (input._2 == 0) List(input._1.toLong, input._2.toLong) else List(input._1.toLong)
        State(program, 0, inputs)
      })

      while (!amplifiers.forall(_.halted)) {
        amplifiers = amplifiers.map(amp => Computer.run(amp.resume))
        amplifiers = updateAmplifiers(amplifiers)
      }

      allOutputs = allOutputs.concat(amplifiers.map(_.inputs).head) // because last output became first input
    }

    allOutputs.max
  }

  def run: Unit = {
    val answer_1 = findMaxSignal1(program)
    val answer_2 = findMaxSignal2(program)

    println(answer_1)
    println(answer_2)
  }

  run
}
