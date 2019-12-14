import Intcode.{Computer, State}

import scala.io.Source

object Day11 extends App {
  val program = Source.fromResource("Day11Input").mkString

  def getNewDirection(currentDirection: Int, turnDirection: Int): Int = {
    // 0 == turn left, 1 == turn right
    turnDirection match {
      case 0 => (currentDirection + 3) % 4
      case 1 => (currentDirection + 1) % 4
    }
  }

  def getNewPosition(currentPosition: (Int, Int),
                     direction: Int): (Int, Int) = {
    val directions = Map(0 -> (0, -1), 1 -> (1, 0), 2 -> (0, 1), 3 -> (-1, 0))
    val newX = currentPosition._1 + directions(direction)._1
    val newY = currentPosition._2 + directions(direction)._2
    (newX, newY)
  }

  def getMap(program: String): Map[(Int, Int), Int] = {
    var map: Map[(Int, Int), Int] = Map()

    // 0 -> "UP", 1 -> "RIGHT", 2 -> "DOWN", 3 -> "LEFT"
    var currentDirection = 0
    var currentPosition = (0, 0)
    var currentColor = 1
    var state = State(program, 0, List(currentColor))

    while (!state.halted) {
      state = Computer.run(state)

      if (state.outputs.length != 2) throw new Exception("Bad outputs")

      val color = state.outputs.head.toInt
      val turnDirection = state.outputs(1).toInt

      map = map.updated(currentPosition, color)

      currentDirection = getNewDirection(currentDirection, turnDirection)
      currentPosition = getNewPosition(currentPosition, currentDirection)
      currentColor = try { map(currentPosition) } catch { case _: NoSuchElementException => 0 }

      state = state.updateInputs(List(currentColor)).updateOutputs(List()).resume
    }

    map
  }

  def printRegIdentifier(map: Map[(Int, Int), Int]): Unit = {
    val width: Int = map.map(_._1._1).max + 1
    val height: Int = map.map(_._1._2).max + 1

    var list: List[List[Char]] = List.fill(height)(List.fill(width)(' '))

    for ((coordinates, color) <- map) {
      if (color == 1) {
        val (y, x) = coordinates
        list = list.updated(x, list(x).updated(y, '●'))
      }
    }

    for (l <- list) println(l.mkString)
  }

  def run: Unit = {
    val map = getMap(program)

    printRegIdentifier(map)
  }

  run
}
