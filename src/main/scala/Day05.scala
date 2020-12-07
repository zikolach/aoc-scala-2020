import scala.io.Source

object Day05 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day05sample.txt"))
    ex1(readLines("day05.txt"))
    ex2(readLines("day05.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def decode(pass: String): Int = {
    def decodeChar(char: Char): Int = char match {
      case 'F' => 0
      case 'B' => 1
      case 'L' => 0
      case 'R' => 1
    }

    val row: Int = pass.substring(0, 7)
      .reverse
      .zipWithIndex
      .map({ case (char, index) => decodeChar(char) * Math.pow(2, index).toInt })
      .sum
    val seat: Int = pass.substring(7).reverse.zipWithIndex.map({ case (char, index) => decodeChar(char) * Math.pow(2, index).toInt }).sum
    // println(s"row: $row, seat: $seat")
    row * 8 + seat
  }

  def ex1(input: Seq[String]): Unit = {
    println(input.map(decode).max)
  }

  def ex2(input: Seq[String]): Unit = {
    val seats = input.map(decode).toSet
    val yourSeat = (0 to (128 * 8)).find(n => seats.contains(n - 1) && seats.contains(n + 1) && !seats.contains(n))
    println(yourSeat)
  }

}
