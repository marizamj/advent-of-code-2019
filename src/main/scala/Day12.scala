object Day12 extends App {
  type Coordinates = (Int, Int, Int)
  type MoonsState = (List[Coordinates], List[Coordinates])

  def getChange(a: Int, b: Int): Int = if (a < b) 1 else if (a == b) 0 else -1

  def getAllChanges(a: Coordinates, b: Coordinates): Coordinates =
    (getChange(a._1, b._1), getChange(a._2, b._2), getChange(a._3, b._3))

  def addChanges(a: Coordinates, b: Coordinates): Coordinates =
    (a._1 + b._1, a._2 + b._2, a._3 + b._3)

  def calcEnergy(coordinates: Coordinates): Int =
    coordinates._1.abs + coordinates._2.abs + coordinates._3.abs

  def getNewVelocity(moons: List[Coordinates],
                     velocity: List[Coordinates]): List[Coordinates] = {
    val gravity: List[Coordinates] = moons.map(moon =>
      moons.map(moon2 => getAllChanges(moon, moon2)).foldLeft((0, 0, 0))((a, b) => addChanges(a, b)))

    velocity.zipWithIndex.map(vi => addChanges(vi._1, gravity(vi._2)))
  }


  def timeStep(state: MoonsState): MoonsState = {
    val (moons, velocity) = state
    val newVelocity = getNewVelocity(moons, velocity)
    val newMoons = moons.zipWithIndex.map(mi => addChanges(mi._1, newVelocity(mi._2)))
    (newMoons, newVelocity)
  }

  def calcTotalEnergy(moons: List[Coordinates], steps: Int): Int = {
    var state = (moons, List.fill(moons.length)((0, 0, 0)))

    for (_ <- 1 to steps) state = timeStep(state)

    val pot = state._1.map(calcEnergy)
    val kin = state._2.map(calcEnergy)

    pot.zip(kin).map(e => e._1 * e._2).sum
  }

  def gcd(list: List[Long]): Long =
    list.tail.foldLeft(list.head)((n, m) => if (m == 0) n.abs else gcd(List(m, n % m)))

  def lcm(list: List[Long]): Long =
    list.tail.foldLeft(list.head)((n, m) => (n * m).abs / gcd(List(n, m)))

  def calcStepsUntilRepeat(moons: List[Coordinates]): Long = {
    var state = (moons, List.fill(moons.length)((0, 0, 0)))
    var steps: Long = 0
    var repeated = false

    var acc: Map[MoonsState, Long] = Map()

    while (steps < 200000) {
      state = timeStep(state)
      steps = steps + 1
      acc = acc.updated(state, steps)

      if (state._1(0)._1 == moons(0)._1 && state._2(0)._1 == 0) println(s"moon 0, x - $steps", steps % 186028)
//      if (state._1(0)._2 == moons(0)._2 && state._2(0)._2 == 0) println(s"moon 0, y - $steps", steps % 161428)
//      if (state._1(0)._3 == moons(0)._3 && state._2(0)._3 == 0) println(s"moon 0, z - $steps", steps % 144624)

//      if (state._1 == moons && state._2 == List.fill(moons.length)((0, 0, 0))) repeated = true
    }

    steps
  }



  def run: Unit = {
    val moons = List((5, 13, -3), (18, -7, 13), (16, 3, 4), (0, 8, 8))
    val test = List((-1, 0, 2), (2, -10, -7), (4, -8, 8), (3, 5, -1))
    val test2 = List((-8, -10, 0), (5, 5, 10), (2, -7, 3), (9, -8, -3))

//    println(lcm(List(186028, 161428, 144624))) // 271442326847376

//    val totalEnergy = calcTotalEnergy(moons, 1000)
    val stepsUntilRepeat = calcStepsUntilRepeat(moons)

//    println(totalEnergy)
    println(stepsUntilRepeat)

//    println(114, 1399, 82)
//    println(gcd(List(114, 1399, 82)))
//    println(lcm(List(114, 1399, 82)))

//      println(2028L * 5898L * 4702L)
//      println(56241299088L / 12 == 4686774924L)
//      println(4686774924L / 12) // 390564577

//    println(186028L * 161428L * 144624L)
//    println(4343077229558016L / 12)

    // 4686774924
    // 56241299088

    // final right answer 271442326847376
  }

  run
}
