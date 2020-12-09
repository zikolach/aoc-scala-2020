import scala.annotation.tailrec
import scala.io.Source

object Day09 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day09sample.txt"))
    //    ex1(readLines("day09.txt"))
    //    ex2(readLines("day07sample2.txt"))
    //    ex2(readLines("day09.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }


  def ex1(input: Seq[String]): Unit = {
  }

  def ex2(input: Seq[String]): Unit = {
  }

}
