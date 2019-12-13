import scala.io.Source

object Day01 extends App {
  val input = Source.fromResource("Day01Input").getLines.map(_.toInt).toList

  def calcFuelRequired(n: Int): Int = n / 3 - 2

  def calcFuelRequiredForFuel(n: Int): Int = {
    val required = calcFuelRequired(n)
    if (required <= 0) 0
    else required + calcFuelRequiredForFuel(required)
  }

  def calcFuelRequiredTotal1(input: List[Int]): Int = {
    if (input.isEmpty) 0
    else calcFuelRequired(input.head) + calcFuelRequiredTotal1(input.tail)
  }

  def calcFuelRequiredTotal2(input: List[Int]): Int = {
    if (input.isEmpty) 0
    else {
      val fuel = calcFuelRequired(input.head)
      val fuelForFuel = calcFuelRequiredForFuel(fuel)
      fuel + fuelForFuel + calcFuelRequiredTotal2(input.tail)
    }
  }

  def run: Unit = {
    val answer1 = calcFuelRequiredTotal1(input)
    val answer2 = calcFuelRequiredTotal2(input)

    println(answer1)
    println(answer2)
  }

  run
}
