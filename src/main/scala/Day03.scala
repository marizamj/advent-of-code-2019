import scala.io.Source

object Day03 extends App {
  val input = Source.fromResource("Day03Input").getLines.toList

  def getPath(line: String): List[(String, String)] = line.split(",").map(_.splitAt(1)).toList

  def getAllCoordinates(path: List[(String, String)],
                        current: (Int, Int, Int) = (0, 0, 0),
                        coordinates: Map[String, Int] = Map.empty): Map[String, Int] = {

    if (path.isEmpty) return coordinates

    val direction = path.head._1
    val distance = path.head._2.toInt

    var newX = current._1
    var newY = current._2
    var newSteps = current._3
    var newCoordinates: Map[String, Int] = coordinates

    for (_ <- 1 to distance) {
      if (direction == "R") newX += 1
      if (direction == "D") newY -= 1
      if (direction == "L") newX -= 1
      if (direction == "U") newY += 1
      newSteps += 1
      newCoordinates = newCoordinates.updated(s"$newX,$newY", newSteps)
    }

    getAllCoordinates(path.tail, (newX, newY, newSteps), newCoordinates)
  }

  def findIntersections(coordinates: List[Map[String, Int]]): List[String] = {
    coordinates(0).keySet.toList.filter(c => coordinates(1).contains(c))
  }

  def findClosestDistance(input: List[String]): Int = {
    val paths = input.map(getPath(_))
    val coordinates = paths.map(getAllCoordinates(_))
    val intersections = findIntersections(coordinates)

    intersections.map(i => i.split(",").map(_.toInt.abs).sum).min
  }

  def findFastestDistance(input: List[String]): Int = {
    val paths = input.map(getPath(_))
    val coordinates = paths.map(getAllCoordinates(_))
    val intersections = findIntersections(coordinates)

    intersections.map(i => coordinates(0)(i) + coordinates(1)(i)).min
  }

  def run: Unit = {
    val answer_1 = findClosestDistance(input)
    val answer_2 = findFastestDistance(input)

    println(answer_1)
    println(answer_2)
  }

  run
}
