import scala.io.Source
import scala.util.Try

object Day14 {

  def main(args: Array[String]): Unit = {
    ex1(readLines("day14sample.txt"))
    ex1(readLines("day14.txt"))
    ex2(readLines("day14sample2.txt"))
    ex2(readLines("day14.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  val maskPatt = "mask = ([X10]{36})".r
  val memPatt = "mem\\[(\\d+)\\] = (\\d+)".r

  def ex1(input: Seq[String]): Unit = {
    val res = input.foldLeft((Map.empty[Long, Long], Option.empty[(Long, Long)])) { (acc, next) =>
      next match {
        case maskPatt(mask) =>
          val orMask = BigInt(mask.replace('X', '0'), 2).longValue
          val andMask = BigInt(mask.replace('X', '1'), 2).longValue
          acc.copy(_2 = Some((orMask, andMask)))
        case memPatt(address, value) =>
          acc._2 match {
            case Some((orMask, andMask)) =>
              acc.copy(_1 = acc._1.updated(address.toLong, (value.toLong | orMask) & andMask))
            case _ => throw new Exception("Unexpected")
          }
        case other => throw new Exception(s"[$other] cannot be matched")
      }
    }
    println(res._1.values.sum)
  }

  def ex2(input: Seq[String]): Unit = {
    val res = input.foldLeft((Map.empty[Long, Long], Option.empty[Seq[(Long, Long)]])) { (acc, next) =>
      next match {
        case maskPatt(mask) =>
          val n = mask.count(_ == 'X')
          val mask2 = mask.replaceAll("0", "Y")
          val ccc = Range(0, Math.pow(2, n).toInt).map(c => {
            val cc = c.toBinaryString.reverse.padTo(n, '0').reverse
            val modMask = cc.foldLeft(mask2)((m, next) => m.replaceFirst("X", next.toString)).replaceAll("Y", "X")
            val orMask = BigInt(modMask.replace('X', '0'), 2).longValue
            val andMask = BigInt(modMask.replace('X', '1'), 2).longValue
            orMask -> andMask
          })
          acc.copy(_2 = Some(ccc))
        case memPatt(address, value) =>
          acc._2 match {
            case Some(masks) =>
              val upd = masks.foldLeft(acc._1)((mem, next) => {
                val (orMask, andMask) = next
                mem.updated((address.toLong | orMask) & andMask, value.toLong)
              })
              acc.copy(_1 = upd)
            case _ => throw new Exception("Unexpected")
          }
        case other => throw new Exception(s"[$other] cannot be matched")
      }
    }
    println(res._1.values.sum)
  }
}
