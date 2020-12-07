import scala.io.Source

object Day06 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day06sample.txt"))
    ex1(readLines("day06.txt"))
    ex2(readLines("day06.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def ex1(input: Seq[String]): Unit = {
    input.foreach(println)
    val groups = input.foldLeft(List.empty[List[String]])((acc, next) =>
      if (next.isEmpty) List.empty :: acc
      else acc match {
        case Nil => (next :: Nil) :: Nil
        case _ => (next :: acc.headOption.getOrElse(Nil)) :: acc.tail
      }
    )
    println(groups)
    val res = groups
      .map(group => group.map(_.toSet).reduce(_ union _))
      .map(_.size)
    println(res)
    println(res.sum)
  }

  def ex2(input: Seq[String]): Unit = {
    input.foreach(println)
    val groups = input.foldLeft(List.empty[List[String]])((acc, next) =>
      if (next.isEmpty) List.empty :: acc
      else acc match {
        case Nil => (next :: Nil) :: Nil
        case _ => (next :: acc.headOption.getOrElse(Nil)) :: acc.tail
      }
    )
    println(groups)
    val res = groups
      .map(group => group.map(_.toSet).reduce(_ intersect _))
      .map(_.size)
    println(res)
    println(res.sum)
  }

}
