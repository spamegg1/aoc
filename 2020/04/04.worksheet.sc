object DataDefs:
  type Chunk = String
  type Field = (String, String)

  val eyeColors  = Seq("amb", "blu", "brn", "gry", "grn", "hzl", "oth")
  val hairColors = ('a' to 'f') ++ ('0' to '9')

  case class Passport(fields: Map[String, String]):
    private lazy val validateFields = fields.map: (field, value) =>
      field match
        case "byr" => value.toIntOption.map(i => 1920 <= i && i <= 2002)
        case "iyr" => value.toIntOption.map(i => 2010 <= i && i <= 2020)
        case "eyr" => value.toIntOption.map(i => 2020 <= i && i <= 2030)
        case "hgt" =>
          value match
            case s"${size}cm" => size.toIntOption.map(i => 150 <= i && i <= 193)
            case s"${size}in" => size.toIntOption.map(i => 59 <= i && i <= 76)
            case _            => None
        case "hcl" =>
          value match
            case s"#$chars" =>
              Some(chars.size == 6 && chars.forall(hairColors.contains(_)))
            case _ => None
        case "ecl" => Some(eyeColors.contains(value))
        case "pid" => Some(value.size == 9 && value.forall(_.isDigit))
        case "cid" => Some(true)
        case _     => Some(true)

    lazy val valid1: Boolean = fields.filterNot((k, _) => k == "cid").size == 7
    lazy val valid2: Boolean = valid1 && validateFields.forall(_.getOrElse(false))

object Parsing:
  import DataDefs.*

  private def parseField(pair: String): Field = pair match
    case s"$field:$value" => field -> value

  private def parseChunk(chunk: Chunk): Passport = Passport:
    chunk
      .split(" ")
      .map(parseField)
      .toMap

  private def parseFile(lines: Seq[String]): Seq[Chunk] = lines
    .map(line => if line.isEmpty then "XXX" else line)
    .mkString(" ")
    .split(" XXX ")
    .toSeq

  def parse(lines: Seq[String]): Seq[Passport] = parseFile(lines).map(parseChunk)

object Solving:
  def solve1(lines: Seq[String]) = Parsing.parse(lines).count(_.valid1)
  def solve2(lines: Seq[String]) = Parsing.parse(lines).count(_.valid2)

object Test:
  lazy val file  = os.pwd / "2020" / "04" / "04.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 2
// Test.res2 // part 2:

object Main:
  lazy val file  = os.pwd / "2020" / "04" / "04.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 210
// Main.res2 // part 2: 131
