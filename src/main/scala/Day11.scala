import scala.collection.immutable.Queue
import scala.io.Source

object Day11 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day11sample.txt"))
    ex1(readLines("day11.txt"))
    ex2(readLines("day11sample.txt"))
    ex2(readLines("day11.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }


  def next(state: Map[(Int, Int), Char])(pos: (Int, Int)): Char = {
    if (state(pos) == '.') '.'
    else {
      val (x, y) = pos
      val neighbours = for {
        xx <- (x - 1) to (x + 1)
        yy <- (y - 1) to (y + 1)
        if xx != x || yy != y
      } yield state.getOrElse((xx, yy), '.')
      val occupied = neighbours.count(_ == '#')
      if (occupied >= 4) 'L'
      else if (occupied == 0) '#'
      else state(pos)
    }
  }

  val dirs: Seq[((Int, Int), Int) => (Int, Int)] = Seq(
    (p, i) => (p._1, p._2 - i),
    (p, i) => (p._1 + i, p._2 - i),
    (p, i) => (p._1 + i, p._2),
    (p, i) => (p._1 + i, p._2 + i),
    (p, i) => (p._1, p._2 + i),
    (p, i) => (p._1 - i, p._2 + i),
    (p, i) => (p._1 - i, p._2),
    (p, i) => (p._1 - i, p._2 - i)
  )

  def next2(state: Map[(Int, Int), Char], maxDist: Seq[Int])(pos: (Int, Int)): Char = {
    if (state(pos) == '.') '.'
    else {
      val occupied = dirs.count(dir => maxDist.map(i => state.getOrElse(dir(pos, i), '.')).find(_ != '.').contains('#'))
      if (occupied >= 5) 'L'
      else if (occupied == 0) '#'
      else state(pos)
    }
  }

  def printState(state: Map[(Int, Int), Char]): Unit = {
    val yy = state.keys.map(_._2).toSeq.sorted
    val xx = state.keys.map(_._1).toSeq.sorted
    yy.foreach { y =>
      xx.foreach { x =>
        print(state((x, y)))
      }
      println()
    }
  }

  def step(state: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
    val m = next(state) _
    state.map { case (xy@(x, y), _) => xy -> m(x, y) }
  }

  def step2(maxDist: Seq[Int])(state: Map[(Int, Int), Char]): Map[(Int, Int), Char] = {
    val m = next2(state, maxDist) _
    state.map { case (xy@(x, y), _) => xy -> m(x, y) }
  }

  def ex1(input: Seq[String]): Unit = {
    val init = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (seat, x) =>
        (x, y) -> seat
      }
    }.toMap

    var i = 0
    val list = LazyList.iterate(init)(step)
    val finalState = list.zip(list.tail).takeWhile(x => x._1 != x._2)
      .map(x => {
        println(i)
        i += 1
        x._2
      })
      .last

    println(s"======== FINAL =========")
    printState(finalState)

    println(s"occupied ${finalState.values.count(_ == '#')} seats")

  }

  def ex2(input: Seq[String]): Unit = {
    val init = input.zipWithIndex.flatMap { case (line, y) =>
      line.zipWithIndex.map { case (seat, x) =>
        (x, y) -> seat
      }
    }.toMap

    var i = 0

    val (xx, yy) = init.keys.unzip
    val (minX, maxX, minY, maxY) = (xx.min, xx.max, yy.min, yy.max)
    val maxDist: Seq[Int] = 1 to Math.max(maxX - minX, maxY - minY)

    val list = LazyList.iterate(init)(step2(maxDist))
    val finalState = list.zip(list.tail).takeWhile(x => x._1 != x._2)
      .map(x => {
        println(s"===== $i =====")
        printState(x._2)
        i += 1
        x._2
      })
      .last

    println(s"======== FINAL =========")
    printState(finalState)

    println(s"occupied ${finalState.values.count(_ == '#')} seats")
  }
}
