
import scala.io.Source

object Day16 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day16sample.txt"))
    ex1(readLines("day16.txt"))
    ex2(readLines("day16sample2.txt"))
    ex2(readLines("day16.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  val patt = "(.+): (\\d+)-(\\d+) or (\\d+)-(\\d+)".r

  def ex1(input: Seq[String]): Unit = {
    val (rules, yourTicket, otherTickets) = parseInput(input)
    val inAnyRange = rules.foldLeft[Int => Boolean](_ => false)((acc, next) => x => acc(x) || next._2.contains(x) || next._3.contains(x))
    val res = otherTickets.flatMap(_.filterNot(x => inAnyRange(x)))
    println(res.sum)
  }

  def parseInput(input: Seq[String]): (Seq[(String, Range.Inclusive, Range.Inclusive)], Seq[Int], Seq[Seq[Int]]) = {
    val Seq(a, b) = input.zipWithIndex.filter(_._1.matches("(your ticket:|nearby tickets:)")).map(_._2)
    val rules = input.take(a).filter(_.nonEmpty).map {
      case patt(a, b, c, d, e) => (a, b.toInt to c.toInt, d.toInt to e.toInt)
    }
    val yourTicket = input.slice(a + 1, a + 1 + b - a - 1).filter(_.nonEmpty).head.split(",").toSeq.map(_.toInt)
    val otherTickets = input.drop(b + 1).filter(_.nonEmpty).map(_.split(",").toSeq.map(_.toInt))
    println(rules)
    println(yourTicket)
    println(otherTickets)
    (rules, yourTicket, otherTickets)
  }

  def ex2(input: Seq[String]): Unit = {
    val (rules, yourTicket, otherTickets) = parseInput(input)
    val inAnyRange = rules.foldLeft[Int => Boolean](_ => false)((acc, next) => x => acc(x) || next._2.contains(x) || next._3.contains(x))
    val validTickets = otherTickets.filter(_.forall(inAnyRange))
    val rulesMap = rules.map(x => x._1 -> (x._2, x._3)).toMap
    val res = validTickets.foldLeft(rulesMap.keys.map(_ -> (1 to yourTicket.size).toSet).toMap) { (acc, next) =>
      acc.map { case (ruleName, nums) =>
        val rule = rulesMap(ruleName)
        val possiblePositions = nums.filter { n =>
          val v = next(n - 1)
          rule._1.contains(v) || rule._2.contains(v)
        }
        ruleName -> possiblePositions
      }
    }
    val sortedByPP = res.toSeq.sortBy(_._2.size)
    val reduced = sortedByPP.tail.zip(sortedByPP).map {
      case ((ruleName, pp), (_, ppp)) => ruleName -> (pp -- ppp).head
    }
    val positions = reduced.prepended(sortedByPP.head._1 -> sortedByPP.head._2.head)
    val finalRes = positions.filter(_._1.startsWith("departure")).map(x => yourTicket(x._2 - 1))
    println("ex2")
    println(positions)
    println(finalRes.map(_.toLong).product)
  }
}
