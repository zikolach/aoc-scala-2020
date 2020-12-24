
import java.util.regex.{Matcher, Pattern}
import scala.annotation.tailrec
import scala.io.Source

object Day18 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day18sample.txt"))
    ex1(readLines("day18.txt"))
    ex2(readLines("day18sample2.txt"))
    ex2(readLines("day18.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  val parens = ".*\\(([^()]+)\\).*".r
  val stmt = "^(\\s*(\\d+)\\s*([+*])\\s*(\\d+)).*".r

  @tailrec
  def calc(expr: String): String = {
    //    println(s"calc(${expr})")
    expr match {
      case stmt(w, a, "+", b) => calc(s"${BigInt(a) + BigInt(b)}${expr.stripPrefix(w)}")
      case stmt(w, a, "*", b) => calc(s"${BigInt(a) * BigInt(b)}${expr.stripPrefix(w)}")
      case _ => BigInt(expr.trim).toString
    }
  }

  @tailrec
  def iter(line: String): String = {
    line match {
      case parens(expr) => iter(line.replace(s"($expr)", calc(expr)))
      case _ => calc(line)
    }
  }

  val sum = "(?:^|\\s|\\*)(\\d+\\s*\\+\\s*\\d+)(?:$|\\s|\\*)".r
  val mul = "(?:^|\\s)(\\d+\\s*\\*\\s*\\d+)(?:$|\\s)".r

  implicit class StrEx(str: String) {
    def replaceFirstStr(target: String, replacement: String): String =
      Pattern.compile(target, Pattern.LITERAL).matcher(str).replaceFirst(Matcher.quoteReplacement(replacement))
  }

  //@tailrec
  def iter2(line: String): String = {
    // println(line)
    line match {
      case parens(expr) => iter2(line.replace(s"($expr)", iter2(expr)))
      case _ =>
        sum.findFirstMatchIn(line) match {
          case Some(s) =>
            iter2(line.replaceFirstStr(s.group(1), calc(s.group(1))))
          case None => mul.findFirstMatchIn(line) match {
            case Some(m) =>
              iter2(line.replaceFirstStr(m.group(1), calc(m.group(1))))
            case None => calc(line)
          }
        }
    }
  }

  def ex1(input: Seq[String]): Unit = {
    val res = input.map { line =>
      val n = iter(line).toLong
      println(s"$line  =  $n")
      n
    }.sum
    println(s"res = $res")
  }

  def ex2(input: Seq[String]): Unit = {
    val res = input.map { line =>
      // println("===========")
      val n = BigInt(iter2(line))
      println(s"$line  =  $n")
      n
    }.sum
    println(s"res = $res")
  }
}
