import scala.annotation.tailrec
import scala.io.Source

object Day08 extends App {
  val input = Source.fromResource("Day08Input").mkString

  def splitIntoLayers(input: String, width: Int, height: Int): List[String] =
    input.grouped(width * height).toList

  def findLayerWithFewestZeros(input: String, width: Int, height: Int): String = {
    val layers = splitIntoLayers(input, width, height)
    val zeros = layers.map(layer => layer.filter(_.toString == "0").length)
    layers.zip(zeros).find(l => l._2 == zeros.min).getOrElse(("0", 0))._1
  }

  def findChecksum(input: String, width: Int, height: Int): Int = {
    val layerWithFewestZeros = findLayerWithFewestZeros(input, width, height)
    val ones = layerWithFewestZeros.filter(_ == '1').length
    val twos = layerWithFewestZeros.filter(_ == '2').length
    ones * twos
  }

  @tailrec
  def zipLayers(acc: List[String], layers: List[String]): List[String] = {
    if (layers.isEmpty) acc
    else zipLayers(acc.zip(layers.head).map(tuple => tuple._1 + tuple._2), layers.tail)
  }

  def decodeImage(input: String, width: Int, height: Int): String = {
    val layers = splitIntoLayers(input, width, height)
    val zipped = zipLayers(layers.head.toList.map(_.toString), layers.tail)
    zipped.map(p => p.find(_.asDigit < 2).getOrElse('2')).mkString
  }

  def decodeAndPrintImage(input: String, width: Int, height: Int): Unit = {
    decodeImage(input, width, height).grouped(width).foreach(line => {
      println(line.replace("1", "+").replace("0"," "))
    })
  }

  def run: Unit = {
    val answer1 = findChecksum(input, 25, 6)

    println(answer1)

    decodeAndPrintImage(input, 25, 6)
  }

  run
}
