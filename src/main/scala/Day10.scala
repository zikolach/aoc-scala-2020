import scala.collection.immutable.Queue
import scala.io.Source

object Day10 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day10sample.txt").map(_.toLong))
    ex1(readLines("day10sample2.txt").map(_.toLong))
    ex1(readLines("day10.txt").map(_.toLong))

    ex2(readLines("day10sample.txt").map(_.toLong))
    ex2(readLines("day10sample2.txt").map(_.toLong))

    // optimized
    ex2a(readLines("day10sample.txt").map(_.toLong))
    ex2a(readLines("day10sample2.txt").map(_.toLong))
    ex2a(readLines("day10.txt").map(_.toLong))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }


  def ex1(input: Seq[Long]): Unit = {
    val sortedInput = input.sorted
    val device = sortedInput.last + 3L
    val res = (0L +: sortedInput).zip(sortedInput :+ device).map(x => x._2 - x._1).groupBy(identity).view.mapValues(_.size)
    println(res.toList)
    println(res.values.product)
  }

  def ex2(input: Seq[Long]): Unit = {
    val sorted = input.sorted
    val combinations = (sorted :+ (sorted.last + 3)).foldLeft(Seq(Queue(0L))) { (acc, next) =>
      val compat = acc.filter(_.last >= next - 3)
      (compat ++ compat.map(_ :+ next)).filter(_.last > next - 3)
    }
    println(combinations.size)
  }


  def ex2a(input: Seq[Long]): Unit = {
    val sorted = input.sorted
    val combinations = (sorted :+ (sorted.last + 3)).foldLeft(Seq(0L -> 1L)) { (acc, next) =>
      val compat = acc.filter(_._1 >= next - 3)
      compat.filter(_._1 > next - 3) :+ next -> compat.map(_._2).sum
    }
    println(combinations.head._2)
  }

}
