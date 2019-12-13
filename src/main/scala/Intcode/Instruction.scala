package Intcode

class Instruction(val program: Map[Long, Long], val pointer: Long, val relativeBase: Long) {
  private val list = program(pointer).toString.map(_.asDigit).toList.reverse.padTo(5, 0)
  val opcode: Int = if (list(1) == 9) 99 else list.head

  val paramModes: List[Int] = list.drop(2)
  // 0 == position mode
  // 1 == immediate mode
  // 2 == relative mode

  def getParam(index: Int): Long = try {
    val paramMode = paramModes(index - 1)
    val paramPointer = program(pointer + index)

    if (opcode == 3) return if (paramMode == 2) paramPointer + relativeBase else paramPointer

    index match {
      case 3 => if (paramMode == 2) paramPointer + relativeBase else paramPointer
      case _ =>
        if (paramMode == 1) paramPointer
        else if (paramMode == 2) program(paramPointer + relativeBase)
        else program(paramPointer)
    }
  } catch { case _: NoSuchElementException => 0 }
}

object Instruction {
  def apply(program: Map[Long, Long], pointer: Long, relativeBase: Long): Instruction =
    new Instruction(program, pointer, relativeBase)
}