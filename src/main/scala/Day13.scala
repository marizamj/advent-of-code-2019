import Intcode.{Computer, State}

import scala.annotation.tailrec
import scala.io.Source

object Day13 extends App {
  val program = Source.fromResource("Day13Input").mkString
  val program2 = program.updated(0, '2')

  def getMap(programOutputs: List[Long]): Map[(Long, Long), Long] =
    programOutputs.grouped(3).foldLeft[Map[(Long, Long), Long]](Map())((acc, i) => acc.updated((i.head, i(1)), i.last))

  def countTiles(programOutputs: List[Long], tile: Int): Int =
    getMap(programOutputs).count(_._2 == tile)

  def countBlocks(programOutputs: List[Long]): Int = countTiles(programOutputs, 2)

  def printState(map: Map[(Long, Long), Long]): Unit = {
    val width: Int = (map.map(_._1._1).max + 1).toInt
    val height: Int = (map.map(_._1._2).max + 1).toInt

    var list: List[List[Char]] = List.fill(height)(List.fill(width)(' '))
    var score: Long = 0

    for ((coordinates, tile) <- map) {
      if (coordinates == (-1,0)) score = tile
      else {
        val (y, x) = coordinates
        val char = tile match {
          case 1 => '■'
          case 2 => '□'
          case 3 => '-'
          case 4 => '●'
          case _ => ' '
        }
        list = list.updated(x.toInt, list(x.toInt).updated(y.toInt, char))
      }
    }

    for (l <- list) println(l.mkString)
    println(s"Score: $score")
  }

  def runArcade(program: String): Long = {
    @tailrec
    def play(state: State): State = {
      if (state.halted || countBlocks(state.outputs) == 0) state
      else {
        val newState = Computer.run(state.resume)
        val map = getMap(newState.outputs)

        printState(map)

        val paddleX: Long = map.find(_._2 == 3).get._1._1
        val ballX: Long = map.find(_._2 == 4).get._1._1
        val input = if (ballX > paddleX) 1 else if (ballX < paddleX) -1 else 0

        play(newState.updateInputs(List(input)))
      }
    }

    val state = Computer.run(State(program))
    val result = play(state)
    getMap(result.outputs).find(_._1 == (-1, 0)).get._2
  }

  def run: Unit = {
    val state1 = Computer.run(State(program))
    val answer1 = countBlocks(state1.outputs)

    println(answer1)

    val answer2 = runArcade(program2)

    println(answer2)
  }

  run
}
