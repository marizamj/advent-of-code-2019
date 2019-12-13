package Intcode

import scala.annotation.tailrec

object Computer {
  @tailrec
  def run(state: State): State =
    if (!state.halted && !state.paused) run(Computer.step(state)) else state

  def step(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)

    instruction.opcode match {
      case 99 => state.halt
      case 1 => add(state)
      case 2 => multiply(state)
      case 3 => saveInput(state)
      case 4 => saveOutput(state)
      case 5 => jumpIfTrue(state)
      case 6 => jumpIfFalse(state)
      case 7 => lessThan(state)
      case 8 => equals(state)
      case 9 => adjustRelativeBase(state)
      case _ => throw new Exception("Unknown opcode")
    }
  }

  def add(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newProgram = state.program.updated(instruction.getParam(3), instruction.getParam(1) + instruction.getParam(2))
    // return new state
    state.updated(newProgram, state.pointer + 4, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def multiply(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newProgram = state.program.updated(instruction.getParam(3), instruction.getParam(1) * instruction.getParam(2))
    // return new state
    state.updated(newProgram, state.pointer + 4, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def saveInput(state: State): State = {
    if (state.inputs.isEmpty) state.pause
    else {
      val instruction = Instruction(state.program, state.pointer, state.relativeBase)
      val newProgram = state.program.updated(instruction.getParam(1), state.inputs.head)
      // return new state
      state.updated(newProgram, state.pointer + 2, state.inputs.tail, state.outputs, state.halted, newPaused = false, state.relativeBase)
    }
  }

  def saveOutput(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newOutputs = state.outputs :+ instruction.getParam(1)
    // return new state
    state.updated(state.program, state.pointer + 2, state.inputs, newOutputs, state.halted, newPaused = false, state.relativeBase)
  }

  def jumpIfTrue(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newPointer = if (instruction.getParam(1) != 0) instruction.getParam(2) else state.pointer + 3
    // return new state
    state.updated(state.program, newPointer, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def jumpIfFalse(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newPointer = if (instruction.getParam(1) == 0) instruction.getParam(2) else state.pointer + 3
    // return new state
    state.updated(state.program, newPointer, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def lessThan(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newValue: Long = if (instruction.getParam(1) < instruction.getParam(2)) 1 else 0
    val newProgram = state.program.updated(instruction.getParam(3), newValue)
    // return new state
    state.updated(newProgram, state.pointer + 4, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def equals(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newValue: Long = if (instruction.getParam(1) == instruction.getParam(2)) 1 else 0
    val newProgram = state.program.updated(instruction.getParam(3), newValue)
    // return new state
    state.updated(newProgram, state.pointer + 4, state.inputs, state.outputs, state.halted, newPaused = false, state.relativeBase)
  }

  def adjustRelativeBase(state: State): State = {
    val instruction = Instruction(state.program, state.pointer, state.relativeBase)
    val newRelativeBase = state.relativeBase + instruction.getParam(1)
    // return new state
    state.updated(state.program, state.pointer + 2, state.inputs, state.outputs, state.halted, newPaused = false, newRelativeBase)
  }
}

