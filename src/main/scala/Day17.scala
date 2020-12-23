
import scala.annotation.tailrec
import scala.io.Source

object Day17 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day17sample.txt"))
    ex1(readLines("day17.txt"))
    ex2(readLines("day17sample.txt"))
    ex2(readLines("day17.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  case class Cell(x: Int, y: Int, z: Int)

  case class Cell4(x: Int, y: Int, z: Int, w: Int)

  def next(state: Cell => Boolean): Cell => Boolean = {
    cell => {
      val active = ((cell.x - 1) to (cell.x + 1))
        .flatMap(x => ((cell.y - 1) to (cell.y + 1))
          .flatMap(y => ((cell.z - 1) to (cell.z + 1))
            .map(z => state(Cell(x, y, z))))).count(x => x)
      if (state(cell)) active >= 3 && active <= 4 else active == 3
    }
  }

  def next4(state: Cell4 => Boolean): Cell4 => Boolean = {
    cell => {
      val active = ((cell.x - 1) to (cell.x + 1))
        .flatMap(x => ((cell.y - 1) to (cell.y + 1))
          .flatMap(y => ((cell.z - 1) to (cell.z + 1))
            .flatMap(z => ((cell.w - 1) to (cell.w + 1))
              .map(w => state(Cell4(x, y, z, w)))))).count(x => x)
      if (state(cell)) active >= 3 && active <= 4 else active == 3
    }
  }

  @tailrec
  def iter(state: Set[Cell], n: Int): Set[Cell] = {
    val xx = state.map(_.x).toSeq.sorted
    val yy = state.map(_.y).toSeq.sorted
    val zz = state.map(_.z).toSeq.sorted
    val nextState = (for {
      x <- (xx.head - 1) to (xx.last + 1)
      y <- (yy.head - 1) to (yy.last + 1)
      z <- (zz.head - 1) to (zz.last + 1)
      cell = Cell(x, y, z)
      if next(state)(cell)
    } yield cell).toSet
    if (n <= 1) nextState else iter(nextState, n - 1)
  }

  @tailrec
  def iter4(state: Set[Cell4], n: Int): Set[Cell4] = {
    val xx = state.map(_.x).toSeq.sorted
    val yy = state.map(_.y).toSeq.sorted
    val zz = state.map(_.z).toSeq.sorted
    val ww = state.map(_.w).toSeq.sorted
    val nextState = (for {
      x <- (xx.head - 1) to (xx.last + 1)
      y <- (yy.head - 1) to (yy.last + 1)
      z <- (zz.head - 1) to (zz.last + 1)
      w <- (ww.head - 1) to (ww.last + 1)
      cell = Cell4(x, y, z, w)
      if next4(state)(cell)
    } yield cell).toSet
    if (n <= 1) nextState else iter4(nextState, n - 1)
  }

  def ex1(input: Seq[String]): Unit = {
    val initial = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.flatMap { case (char, x) =>
        if (char == '#') Some(Cell(x, y, 0)) else None
      }
    }.toSet
    println(s"init: $initial")

    val step1 = iter(initial, 1)
    val step2 = iter(initial, 2)
    val step3 = iter(initial, 3)
    val step6 = iter(initial, 6)
    println(step1.size)
    println(step2.size)
    println(step3.size)
    println(step6.size)
  }

  def ex2(input: Seq[String]): Unit = {
    val initial = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.flatMap { case (char, x) =>
        if (char == '#') Some(Cell4(x, y, 0, 0)) else None
      }
    }.toSet
    println(s"init: $initial")

    val step1 = iter4(initial, 1)
    val step2 = iter4(initial, 2)
    val step3 = iter4(initial, 3)
    val step6 = iter4(initial, 6)
    println(step1.size)
    println(step2.size)
    println(step3.size)
    println(step6.size)
  }
}
