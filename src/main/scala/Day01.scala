import scala.io.Source

object Day01 extends App {
  val input = Source.fromResource("Day01Input").getLines.map(_.toInt).toList

  def calcFuelRequired(n: Int): Int = n / 3 - 2

  def calcFuelRequiredForFuel(n: Int): Int = {
    val required = calcFuelRequired(n)
    if (required <= 0) 0
    else required + calcFuelRequiredForFuel(required)
  }

  def calcFuelRequiredTotal_1(input: List[Int]): Int = {
    if (input.isEmpty) 0
    else calcFuelRequired(input.head) + calcFuelRequiredTotal_1(input.tail)
  }

  def calcFuelRequiredTotal_2(input: List[Int]): Int = {
    if (input.isEmpty) 0
    else {
      val fuel = calcFuelRequired(input.head)
      val fuelForFuel = calcFuelRequiredForFuel(fuel)
      fuel + fuelForFuel + calcFuelRequiredTotal_2(input.tail)
    }
  }

  def run: Unit = {
    val answer_1 = calcFuelRequiredTotal_1(input)
    val answer_2 = calcFuelRequiredTotal_2(input)

    println(answer_1)
    println(answer_2)
  }

  run
}
