import scala.annotation.tailrec
import scala.io.Source

object Day06 extends App {
  val input: String = Source.fromResource("Day06Input").mkString

  def parseInput(input: String): Array[Array[String]] = input.split("\n").map(_.split("\\)"))

  @tailrec
  def getMap(pairs: Array[Array[String]], map: Map[String, String] = Map()): Map[String, String] =
    if (pairs.isEmpty) map
    else getMap(pairs.tail, map.updated(pairs.head(1), pairs.head(0)))

  def countOrbits(input: String): Int = {
    val parsed = parseInput(input)
    val map = getMap(parsed)

    @tailrec
    def count(orbit: String, counter: Int = 0): Int = {
      if (!map.keySet.contains(orbit)) counter
      else count(map(orbit), counter + 1)
    }

    map.keys.toList.map(count(_)).sum
  }

  @tailrec
  def getPathToRoot(orbit: String, map: Map[String, String], path: List[String] = List.empty): List[String] =
    if (!map.keySet.contains(orbit)) path
    else getPathToRoot(map(orbit), map, path.appended(map(orbit)))

  def findShortestDistance(input: String, start: String = "YOU", end: String = "SAN"): Int = {
    val parsed = parseInput(input)
    val map = getMap(parsed)
    val pathToRootFromStart = getPathToRoot(start, map)
    val pathToRootFromEnd = getPathToRoot(end, map)
    val firstIntersection = pathToRootFromStart.find(pathToRootFromEnd.contains(_)).getOrElse(None)

    if (firstIntersection != None) {
      pathToRootFromStart.indexOf(firstIntersection) + pathToRootFromEnd.indexOf(firstIntersection)
    } else throw new Exception("Not found")
  }

  def run: Unit = {
    val answer1 = countOrbits(input)
    val answer2 = findShortestDistance(input)

    println(answer1)
    println(answer2)
  }

  run
}
