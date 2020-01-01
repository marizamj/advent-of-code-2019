import scala.io.Source

object Day14 extends App {
  val input = Source.fromResource("Day14Input").mkString

  type Entry = (String, Int)
  type Instructions = Map[Entry, List[Entry]]

  def parseEntry(input: String): Entry = {
    val arr = input.split(' ')
    (arr.last, arr.head.toInt)
  }

  def parseInput(input: String): Instructions =
    input.split('\n').foldLeft(Map(): Instructions)((acc, line) => {
      val arr = line.split(" => ")
      val in = parseEntry(arr.last)
      val out = arr.head.split(", ").map(parseEntry).toList
      acc.updated(in, out)
    })

//  def convertEntry(entry: Entry, instructions: Instructions): Int = {
//    val (element, amount) = entry
//    instructions(entry).map[Int](e => if (e._1 == "ORE") e._2 else convertEntry(e, instructions)).sum
//  }


  def calculateOre(input: String): Int = {
    val instructions = parseInput(input)

    var list = instructions(("FUEL", 1))

    println(list)

    list = list.flatMap(entry => {
      instructions.find(_._1._1 == entry._1).get._2
    })

    println(list)

    ???
  }


  println(calculateOre(input))
//  println((13.toFloat / 5.toFloat).ceil.toInt) // 2
}
