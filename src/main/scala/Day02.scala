import scala.io.Source

object Day02 extends App {
  val input = Source.fromResource("Day02Input").mkString.split(",").map(_.toInt).toList
  val input_2 = input.updated(1, 12).updated(2, 2)

  @scala.annotation.tailrec
  def runIntcode(input: List[Int], pointer: Int = 0): List[Int] = {
    val opcode = input(pointer)

    if (opcode == 99) input
    else  if (opcode == 1 || opcode == 2) {
      val param1 = input(pointer + 1)
      val param2 = input(pointer + 2)
      val destinationIndex = input(pointer + 3)

      if (opcode == 1) {
        val newInput = input.updated(destinationIndex, input(param1) + input(param2))
        runIntcode(newInput, pointer + 4)
      }
      else {
        val newInput = input.updated(destinationIndex, input(param1) * input(param2))
        runIntcode(newInput, pointer + 4)
      }
    }
    else throw new Exception("Unknown opcode")
  }

  def findPair(input: List[Int], target: Int): Int = {
    def step(pointer1: Int, pointer2: Int): List[Int] = {
      val currentInput = input.updated(1, pointer1).updated(2, pointer2)
      try {
        runIntcode(currentInput)
      } catch {
        case e: Exception => Nil
      }
    }

    for (pointer1 <- 0 to 99) {
      for (pointer2 <- 0 to 99) {
        val result = step(pointer1, pointer2)
        if (result.head == target) return 100 * result(1) + result(2)
      }
    }

    throw new Exception("No result")
  }

  def run: Unit = {
    val answer_1 = runIntcode(input_2)
    val answer_2 = findPair(input, 19690720)

    println(answer_1)
    println(answer_2)
  }

  run
}
