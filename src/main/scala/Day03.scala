import scala.io.Source

object Day03 {
  def main(args: Array[String]): Unit = {
    //    ex1(readLines("day03sample.txt"))
    ex1(readLines("day03.txt"))
    //    ex2(readLines("day03sample.txt"))
    ex2(readLines("day03.txt"))
  }

  private def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def ex1(input: Seq[String]): Unit = {
    println(countTrees(input, 3, 1))
  }

  private def countTrees(input: Seq[String], right: Int, down: Int) = {
    input.zipWithIndex.count {
      case (line, index) if (index % down) == 0 =>
        val pos = (index / down * right) % line.length
        //        println(s"pow = $pos, tree = ${line.charAt(pos)}")
        line.charAt(pos) == '#'
      case _ => false
    }
  }

  def ex2(input: Seq[String]): Unit = {
    val slopes = List(1 -> 1, 3 -> 1, 5 -> 1, 7 -> 1, 1 -> 2)
    val result = slopes.map {
      case (right, down) => countTrees(input, right, down)
    }
    //    println(result)
    println(result.product)
  }
}
