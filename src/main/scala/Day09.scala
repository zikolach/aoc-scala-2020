import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day09sample.txt").map(_.toLong), 5)
    ex1(readLines("day09.txt").map(_.toLong))
    //    ex2(readLines("day07sample2.txt"))
    ex2(readLines("day09.txt").map(_.toLong), 138879426L)
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }


  def ex1(input: Seq[Long], preamble: Int = 25): Unit = {
    val res = (preamble until input.size).find { n =>
      !(n - preamble until n).exists { a =>
        (n - preamble until n)
          .filterNot(_ == a)
          .exists { b =>
            //            println(s"a=$a, b=$b, ${input(a)} + ${input(b)} vs ${input(n)}")
            input(a) + input(b) == input(n)
          }
      }
    }
    println(res.map(input))
  }

  def ex2(input: Seq[Long], sum: Long): Unit = {
    def getSums(n: Int) = {
      val sums = LazyList
        .from(n)
        .map(x => x -> (n to x).map(input).sum)
        .takeWhile(_._2 <= sum)
      sums
    }

    val res = input.indices.find { n =>
      getSums(n).lastOption.exists(_._2 == sum)
    }

    println(res)

    res.foreach { n =>
      val nums = (n until n + getSums(n).size).map(input)
      println(nums)
      println(nums.min + nums.max)
    }
  }

}
