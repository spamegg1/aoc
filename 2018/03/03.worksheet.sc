object DataDefs:
  case class Claim(id: Int, rows: Range, cols: Range):
    def intersect(that: Claim) =
      for
        row <- rows.intersect(that.rows)
        col <- cols.intersect(that.cols)
      yield (row, col)

object Parsing:
  import DataDefs.*

  private def parseLine(line: String) = line match
    case s"#$id @ $row,$col: ${rows}x$cols" =>
      Claim(
        id.toInt,
        Range(row.toInt, row.toInt + rows.toInt),
        Range(col.toInt, col.toInt + cols.toInt)
      )

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .combinations(2)
    .flatMap { case Seq(c1, c2) => c1.intersect(c2) }
    .distinct
    .size

  def solve2(lines: Seq[String]) =
    val claims = Parsing.parse(lines)
    claims
      .find: c1 =>
        claims.forall(c2 => c1 == c2 || c1.intersect(c2).isEmpty)
      .get
      .id

object Test:
  lazy val file  = os.pwd / "2018" / "03" / "03.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 4
// Test.res2 // part 2: 3

object Main:
  lazy val file  = os.pwd / "2018" / "03" / "03.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 110389
// Main.res2 // part 2: 552
