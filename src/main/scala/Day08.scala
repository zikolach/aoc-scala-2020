import scala.annotation.tailrec
import scala.io.Source

object Day08 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day08sample.txt"))
    ex1(readLines("day08.txt"))
    //    ex2(readLines("day07sample2.txt"))
    ex2(readLines("day08.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def ex1(input: Seq[String]): Unit = {
    val patt = "(\\w{3}) ([+-]\\d+)".r
    var acc: Int = 0
    var executed: Set[Int] = Set.empty
    val prog = input

    @tailrec
    def run(index: Int): Unit = {
      if (index == prog.size) {
        println(s"found $acc")
      } else if (executed.contains(index)) {
        println(s"acc = $acc")
      } else {
        executed += index
        val op = prog(index)
        println(op)
        op match {
          case patt(op, arg) if op == "nop" =>
            run(index + 1)
          case patt(op, arg) if op == "acc" =>
            acc += arg.toInt
            run(index + 1)
          case patt(op, arg) if op == "jmp" =>
            run(index + arg.toInt)
        }
      }
    }

    run(0)
  }

  def ex2(input: Seq[String]): Unit = {
    val patt = "(\\w{3}) ([+-]\\d+)".r

    def iter(prog: Seq[String]): Boolean = {
      var acc: Int = 0
      var executed: Set[Int] = Set.empty

      @tailrec
      def run(index: Int): Boolean = {
        if (index == prog.size) {
          println(s"found $acc")
          true
        } else if (executed.contains(index)) {
          println(s"acc = $acc")
          false
        } else {
          executed += index
          val op = prog(index)
          //          println(op)
          op match {
            case patt(op, _) if op == "nop" =>
              run(index + 1)
            case patt(op, arg) if op == "acc" =>
              acc += arg.toInt
              run(index + 1)
            case patt(op, arg) if op == "jmp" =>
              run(index + arg.toInt)
          }
        }
      }

      run(0)
    }


    input.zipWithIndex.find {
      case (patt(op, arg), line) if op == "jmp" =>
        val prog = input.updated(line, s"nop $arg")
        iter(prog)
      case (patt(op, arg), line) if op == "nop" =>
        val prog = input.updated(line, s"jmp $arg")
        iter(prog)
      case _ => false
    }
  }

}
