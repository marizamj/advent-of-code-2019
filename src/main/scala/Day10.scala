object Day10 extends App {
  def findDistance(from: (Int, Int), to: (Int, Int)): (Int, Int) = {
    val (x1, y1) = from
    val (x2, y2) = to
    (x2 - x1, y2 - y1)
  }

  def findSomething(from: (Int, Int), to: (Int, Int)): (Int, Int) = {
    val (x2, y2) = to
    val (x, y) = findDistance(from, to)

    if (x % 2 == 0 && y % 2 == 0) findSomething(from, (x2 - x / 2, y2 - y / 2))
    else (x, y)
  }

  def getMap(input: String): Map[(Int, Int), Char] = {
    val lines = input.split("\n")

    var map: Map[(Int, Int), Char] = Map()

    for ((line, y) <- lines.zipWithIndex) {
      for ((content, x) <- line.zipWithIndex) {
        map = map.updated((x, y), content)
      }
    }

    map.filter(_._2 == '#')
  }

  def calculateVisible(from: (Int, Int), map: Map[(Int, Int), Char]) = {
    var a = map
    map.foreach(entry => {
      if (from != entry._1) {
        val (x, y) = findDistance(from, entry._1)
        if (x == y)
        a = a.updated(entry._1, 'q')
      }
    })
  }

  var map: Map[(Int, Int), Char] = getMap(".#..#\n.....\n#####\n....#\n...##")
//  println(calculateVisible((0, 0), map))
//  println(map)
}
