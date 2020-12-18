import scala.annotation.tailrec
import scala.io.Source

object Day15 {

  def main(args: Array[String]): Unit = {
    ex1()
    ex2()
  }

  @tailrec
  def iter(nums: Map[Int, (Int, Option[Int])], lastN: Int, end: Int): Int = {
    val (lastTime, prevTime) = nums(lastN)
    val nextNum = prevTime.map(lastTime - _).getOrElse(0)
    val n = lastTime + 1
    if (n >= end) nextNum
    else {
      val newNums = nums.updated(nextNum, (n, nums.get(nextNum).map(_._1)))
      iter(newNums, nextNum, end)
    }
  }

  def ex1(): Unit = {
    {
      val input = List(0, 3, 6)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 10)
      println(res) // 0
    }
    {
      val input = List(1, 3, 2)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 2020)
      println(res) // 1
    }
    {
      val input = List(3, 1, 2)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 2020)
      println(res) // 1836
    }
    {
      val input = List(0, 1, 4, 13, 15, 12, 16)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 2020)
      println(res) // ???
    }
  }

  def ex2(): Unit = {
    {
      val input = List(0, 3, 6)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 30000000)
      println(res) // 175594
    }
    {
      val input = List(0, 1, 4, 13, 15, 12, 16)
      val res = iter(input.zipWithIndex.map(p => p._1 -> (p._2 + 1, None)).toMap, input.last, 30000000)
      println(res) // ???
    }
  }
}
