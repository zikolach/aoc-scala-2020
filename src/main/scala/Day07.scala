import scala.annotation.tailrec
import scala.io.Source

object Day07 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day07sample.txt"))
    ex1(readLines("day07.txt"))
    ex2(readLines("day07sample2.txt"))
    ex2(readLines("day07.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  case class Rule(bag: String, content: Map[String, Int])

  val splitBags = "(\\d+ [^,.]+)".r
  val splitBag = "(\\d+) (.+) bags*".r

  def ex1(input: Seq[String]): Unit = {
    val rules = readRules(input)

    @tailrec
    def find(acc: Set[String], bags: Set[String]): Set[String] = {
      val holders = bags.flatMap(bag => rules.filter(_.content.contains(bag)).map(_.bag))
      if (holders.isEmpty) acc
      else {
        println(holders)
        find(acc ++ holders, holders)
      }
    }

    println(find(Set.empty, Set("shiny gold")))
    println(find(Set.empty, Set("shiny gold")).size)
  }

  def readRules(input: Seq[String]): Seq[Rule] = {
    val rules = input.map(_.split(" bags contain ") match {
      case Array(name, contents) =>
        contents match {
          case "no other bags." =>
            Rule(name, Map.empty)
          case _ =>
            //            println(splitBags.findAllIn(contents).toList)
            val contentCounts = splitBags
              .findAllIn(contents)
              .toList
              .map {
                case splitBag(n, name) => name -> n.toInt
                case other => throw new Exception(s"$other is not parsable")
              }
              .toMap
            Rule(name, contentCounts)
        }
    })
    rules.foreach(println)
    rules
  }

  def ex2(input: Seq[String]): Unit = {
    val rules = readRules(input)

    def find(bag: String): Int = {
      rules.find(_.bag == bag) match {
        case Some(Rule(name, content)) if content.isEmpty => 1
        case Some(Rule(name, content)) =>
          1 + content.map(c => find(c._1) * c._2).sum
      }
    }

    println(find("shiny gold") - 1)
  }

}
