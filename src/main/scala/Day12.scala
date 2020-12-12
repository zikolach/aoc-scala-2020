import scala.io.Source
import scala.util.matching.Regex

object Day12 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day12sample.txt"))
    ex1(readLines("day12.txt"))
    ex2(readLines("day12sample.txt"))
    ex2(readLines("day12.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  val cmd: Regex = "(\\w)(\\d+)".r

  def ex1(input: Seq[String]): Unit = {
    val res = input.foldLeft((0, 0, 1)) { (curr, next) =>
      val to = next match {
        case cmd("N", b) => curr.copy(_2 = curr._2 - b.toInt)
        case cmd("E", b) => curr.copy(_1 = curr._1 + b.toInt)
        case cmd("S", b) => curr.copy(_2 = curr._2 + b.toInt)
        case cmd("W", b) => curr.copy(_1 = curr._1 - b.toInt)
        case cmd("L", b) => curr.copy(_3 = (curr._3 + 4 - b.toInt / 90) % 4)
        case cmd("R", b) => curr.copy(_3 = (curr._3 + b.toInt / 90) % 4)
        case cmd("F", b) => curr._3 match {
          case 0 => curr.copy(_2 = curr._2 - b.toInt)
          case 1 => curr.copy(_1 = curr._1 + b.toInt)
          case 2 => curr.copy(_2 = curr._2 + b.toInt)
          case 3 => curr.copy(_1 = curr._1 - b.toInt)
        }
      }
      printState(to)
      to
    }
    println(s"Res ${Math.abs(res._1) + Math.abs(res._2)}")
  }

  def printState(p: (Int, Int, Int)): Unit = {
    val facing = p._3 match {
      case 0 => "north"
      case 1 => "east"
      case 2 => "south"
      case 3 => "west"
    }
    println((if (p._1 <= 0) s"west ${Math.abs(p._1)}, " else s"east ${p._1}, ") +
      (if (p._2 <= 0) s"north ${Math.abs(p._2)}, " else s"south ${p._2}, ") +
      s"facing ${facing}")
  }

  def ex2(input: Seq[String]): Unit = {
    val res = input.foldLeft((10, -1, 1, 0, 0)) { (curr, next) =>
      //      println(curr)
      //      println(next)
      next match {
        case cmd("N", b) => curr.copy(_2 = curr._2 - b.toInt)
        case cmd("E", b) => curr.copy(_1 = curr._1 + b.toInt)
        case cmd("S", b) => curr.copy(_2 = curr._2 + b.toInt)
        case cmd("W", b) => curr.copy(_1 = curr._1 - b.toInt)
        case cmd("F", b) =>
          val times = b.toInt
          curr.copy(_4 = curr._4 + curr._1 * times, _5 = curr._5 + curr._2 * times)
        case cmd(dir, b) =>
          val newDir = dir match {
            case "L" => (curr._3 + 4 - b.toInt / 90) % 4
            case "R" => (curr._3 + b.toInt / 90) % 4
          }
          (newDir - curr._3) match {
            case 1 | -3 => curr.copy(_3 = newDir, _1 = -curr._2, _2 = curr._1)
            case 2 | -2 => curr.copy(_3 = newDir, _1 = -curr._1, _2 = -curr._2)
            case 3 | -1 => curr.copy(_3 = newDir, _1 = curr._2, _2 = -curr._1)
          }
      }
    }
    println(s"Res ${Math.abs(res._4) + Math.abs(res._5)}")
  }
}
