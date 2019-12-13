import scala.annotation.tailrec
import scala.io.Source

object Day05 extends App {
  val program = Source.fromResource("Day05Input").mkString.split(",").map(_.toInt).toList

  def getInstruction(program: List[Int], pointer: Int): List[Int] = {
    program(pointer).toString.map(_.asDigit).toList.reverse.padTo(4, 0)
  }

  @tailrec
  def runIntcode(program: List[Int],
                 pointer: Int = 0,
                 input: Int = 0,
                 output: List[Int] = Nil): List[Int] = {

    val instruction = getInstruction(program, pointer)
    val opcode = instruction.head
    val param1Mode = instruction(2)
    val param2Mode = instruction(3)

    var newProgram = program
    var newPointer = pointer
    var newOutput = output

    if (opcode == 9) return output

    else if (opcode == 3) {
      val destinationIndex = program(pointer + 1)
      newProgram = program.updated(destinationIndex, input)
      newPointer = pointer + 2
    }

    else if (opcode == 4) {
      val out = if (param1Mode == 1) program(pointer + 1) else program(program(pointer + 1))
      newOutput = out :: newOutput
      newPointer = pointer + 2
    }

    else {
      // Instructions with 2 parameters

      val param1 = if (param1Mode == 1) program(pointer + 1) else program(program(pointer + 1))
      val param2 = if (param2Mode == 1) program(pointer + 2) else program(program(pointer + 2))

      if (opcode == 5) {
        if (param1 != 0)newPointer = param2 else newPointer = pointer + 3
      }

      else if (opcode == 6) {
        if (param1 == 0) newPointer = param2 else newPointer = pointer + 3
      }

      else {
        // Instructions with 3 parameters
        newPointer = pointer + 4

        val destinationIndex = program(pointer + 3)

        if (opcode == 1) newProgram = program.updated(destinationIndex, param1 + param2)
        if (opcode == 2) newProgram = program.updated(destinationIndex, param1 * param2)
        if (opcode == 7) {
          val newValue = if (param1 < param2) 1 else 0
          newProgram = program.updated(destinationIndex, newValue)
        }
        if (opcode == 8) {
          val newValue = if (param1 == param2) 1 else 0
          newProgram = program.updated(destinationIndex, newValue)
        }
      }
    }

    runIntcode(program = newProgram, pointer = newPointer, input = input, output = newOutput)
  }

  def run: Unit = {
    println(runIntcode(program = program, input = 5))
  }

  run
}
