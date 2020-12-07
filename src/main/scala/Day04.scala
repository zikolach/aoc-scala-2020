import scala.io.Source

object Day04 {

  case class Pass(
                   byr: Option[String] = None, // (Birth Year)
                   iyr: Option[String] = None, // (Issue Year)
                   eyr: Option[String] = None, // (Expiration Year)
                   hgt: Option[String] = None, // (Height)
                   hcl: Option[String] = None, // (Hair Color)
                   ecl: Option[String] = None, // (Eye Color)
                   pid: Option[String] = None, // (Passport ID)
                   cid: Option[String] = None // (Country ID)
                 ) {
    override def toString: String =
      s"""Pass(byr=[$byr],iyr=[$iyr],eyr=[$eyr],hgt=[$hgt],hcl=[$hcl],ecl=[$ecl],pid=[$pid],cid=[$cid])"""
  }

  def main(args: Array[String]): Unit = {
//    ex1(readLines("day04sample.txt"))
//    ex1(readLines("day04.txt"))
    ex2(readLines("day04valid.txt"))
    ex2(readLines("day04invalid.txt"))
    ex2(readLines("day04.txt"))
  }

  def readLines(filename: String): List[String] = {
    val src = Source.fromFile(getClass.getResource(filename).getPath)
    val lines = src.getLines().toList
    src.close()
    lines
  }

  def ex1(input: Seq[String]): Unit = {
    val passports = readPassports(input)
    val valid = passports.count(validate1)
    println(s"valid = $valid")
  }

  def readPassports(input: Seq[String]): Seq[Pass] = {
    val grouped = input.foldLeft(List.empty[String]) {
      (acc, next) =>
        if (next.isEmpty) "" :: acc
        else acc.headOption.map(_ + " " + next).getOrElse(next) :: (if (acc.isEmpty) Nil else acc.tail)
    }
    val passports = grouped.map { passStr =>
      passStr.split(' ').filter(_.nonEmpty).foldLeft(Pass()) {
        (pass, next) =>
          next.split(':') match {
            case Array("byr", byr) => pass.copy(byr = Some(byr))
            case Array("iyr", iyr) => pass.copy(iyr = Some(iyr))
            case Array("eyr", eyr) => pass.copy(eyr = Some(eyr))
            case Array("hgt", hgt) => pass.copy(hgt = Some(hgt))
            case Array("hcl", hcl) => pass.copy(hcl = Some(hcl))
            case Array("ecl", ecl) => pass.copy(ecl = Some(ecl))
            case Array("pid", pid) => pass.copy(pid = Some(pid))
            case Array("cid", cid) => pass.copy(cid = Some(cid))
          }
      }
    }
    println(passports.take(10))
    passports
  }

  def validate1(pass: Pass): Boolean =
    pass.byr.isDefined &&
      pass.iyr.isDefined &&
      pass.eyr.isDefined &&
      pass.hgt.isDefined &&
      pass.hcl.isDefined &&
      pass.ecl.isDefined &&
      pass.pid.isDefined &&
      true //pass.cid.isDefi
  // ned
  def validate2(pass: Pass): Boolean = {
    println(pass)
    val validByr = pass.byr.exists(x => x.matches("\\d\\d\\d\\d") && x.toInt >= 1920 && x.toInt <= 2002)
    val validIyr = pass.iyr.exists(x => x.matches("\\d\\d\\d\\d") && x.toInt >= 2010 && x.toInt <= 2020)
    val validEyr = pass.eyr.exists(x => x.matches("\\d\\d\\d\\d") && x.toInt >= 2020 && x.toInt <= 2030)
    val validHgt = pass.hgt.exists(x => (x.matches("\\d+cm") && x.dropRight(2).toInt >= 150 && x.dropRight(2).toInt <= 193) || (x.matches("\\d+in") && x.dropRight(2).toInt >= 59 && x.dropRight(2).toInt <= 76))
    val validHcl = pass.hcl.exists(x => x.matches("#[a-f0-9]{6}"))
    val validEcl = pass.ecl.exists(x => Set("amb", "blu", "brn", "gry", "grn", "hzl", "oth").contains(x))
    val validPid = pass.pid.exists(_.matches("\\d{9}"))
    val valid = validByr &&
      validIyr &&
      validEyr &&
      validHgt &&
      validHcl &&
      validEcl &&
      validPid &&
      true //pass.cid.isDefined
    println(valid)
    valid
  }


  def ex2(input: Seq[String]): Unit = {
    val passports = readPassports(input)
    val valid = passports.count(validate2)
    println(s"valid2 = $valid")
  }

}
