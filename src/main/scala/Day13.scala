import scala.io.Source
import scala.util.Try
import scala.util.matching.Regex

object Day13 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day13sample.txt"))
    ex1(readLines("day13.txt"))
    ex2(readLines("day13sample.txt"))
    ex2(readLines("day13.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def ex1(input: Seq[String]): Unit = {
    val time = input.head.toLong
    val buses = input.tail.head.split(',').flatMap(x => Try(x.toLong).toOption).toSet
    println(time)
    println(buses)
    val nextBusTime = buses.map(x => x -> (x - (time % x))).minBy(_._2)
    println(nextBusTime)
    println(nextBusTime._1 * nextBusTime._2)
  }

  def ex2(input: Seq[String]): Unit = {
    val buses = input.tail.head.split(',').zipWithIndex.flatMap(x => Try(x._1.toLong).toOption.map(_ -> x._2)).toSeq
    println(buses)

    val (maxBusInterval, maxBusIndex) = buses.maxBy(_._1)
    println(maxBusInterval)
    val otherBuses = buses.filterNot(_._1 == maxBusInterval)
    val res = Iterator
      .iterate(0L)(n => n + maxBusInterval)
      .find(n => {
        otherBuses.forall {
          case (interval, index) => (n - maxBusIndex + index) % interval == 0
        }
      })
    println(res.map(_ - maxBusIndex))


    /*

    t = i1 * n + a1   --->   n = (t - a1) / i3
    t = i2 * n + a2   --->   n = (t - a2) / i2
    t = i3 * n + a3   --->   n = (t - a3) / i3



    */


  }
}
