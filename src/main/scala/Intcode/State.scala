package Intcode

import scala.annotation.tailrec

class State(val program: Map[Long, Long],
            val pointer: Long,
            val inputs: List[Long],
            val outputs: List[Long],
            val halted: Boolean,
            val paused: Boolean,
            val relativeBase: Long) {

  def pause = new State(program, pointer, inputs, outputs, halted, true, relativeBase)

  def resume = new State(program, pointer, inputs, outputs, halted, false, relativeBase)

  def halt = new State(program, pointer, inputs, outputs, true, false, relativeBase)

  def updated(newProgram: Map[Long, Long],
              newPointer: Long,
              newInputs: List[Long],
              newOutputs: List[Long],
              newHalted: Boolean,
              newPaused: Boolean,
              newRelativeBase: Long) =
    new State(newProgram, newPointer, newInputs, newOutputs, newHalted, newPaused, newRelativeBase)

  def updateInputs(newInputs: List[Long]) = new State(program, pointer, newInputs, outputs, halted, paused, relativeBase)

  def updateOutputs(newOutputs: List[Long]) = new State(program, pointer, inputs, newOutputs, halted, paused, relativeBase)
}

object State {
  def apply(program: String,
            pointer: Long = 0,
            inputs: List[Long] = Nil,
            outputs: List[Long] = Nil,
            halted: Boolean = false,
            paused: Boolean = false,
            relativeBase: Long = 0) =
    new State(parseProgram(program), pointer, inputs, outputs, halted, paused, relativeBase)

  def parseProgram(input: String): Map[Long, Long] = {
    @tailrec
    def getMap(tuples: List[(Long, Int)], acc: Map[Long, Long]): Map[Long, Long] = {
      if (tuples.isEmpty) acc else getMap(tuples.tail, acc.updated(tuples.head._2.toLong, tuples.head._1))
    }
    val tuples = input.split(",").map(_.toLong).zipWithIndex.toList
    getMap(tuples, Map())
  }
}