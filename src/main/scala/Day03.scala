import scala.annotation.tailrec
import scala.io.Source

object Day03 extends App {
  val input = Source.fromResource("Day03Input").getLines.toList

  def getPath(line: String): List[(String, String)] = line.split(",").map(_.splitAt(1)).toList

  @tailrec
  def getAllCoordinates(path: List[(String, String)],
                        current: (Int, Int, Int) = (0, 0, 0),
                        coordinates: Map[String, Int] = Map.empty): Map[String, Int] = {

    if (path.isEmpty) return coordinates

    val (direction, distance) = path.head

    var (newX, newY, newSteps) = current
    var newCoordinates: Map[String, Int] = coordinates

    for (_ <- 1 to distance.toInt) {
      direction match {
        case "R" => newX += 1
        case "D" => newY -= 1
        case "L" => newX -= 1
        case "U" => newY += 1
      }

      newSteps += 1
      newCoordinates = newCoordinates.updated(s"$newX,$newY", newSteps)
    }

    getAllCoordinates(path.tail, (newX, newY, newSteps), newCoordinates)
  }

  def findIntersections(coordinates: List[Map[String, Int]]): List[String] =
    coordinates.head.keySet.toList.filter(c => coordinates(1).contains(c))

  def findClosestDistance(input: List[String]): Int = {
    val paths = input.map(getPath)
    val coordinates = paths.map(getAllCoordinates(_))
    val intersections = findIntersections(coordinates)

    intersections.map(i => i.split(',').map(_.toInt.abs).sum).min
  }

  def findFastestDistance(input: List[String]): Int = {
    val paths = input.map(getPath)
    val coordinates = paths.map(getAllCoordinates(_))
    val intersections = findIntersections(coordinates)

    intersections.map(i => coordinates.head(i) + coordinates(1)(i)).min
  }

  def run: Unit = {
    val answer1 = findClosestDistance(input)
    val answer2 = findFastestDistance(input)

    println(answer1)
    println(answer2)
  }

  run
}
