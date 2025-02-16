object DataDefs:
  type Pattern = Seq[String]
  type Rules   = Map[Pattern, Pattern]

  extension (p: Pattern)
    def rotated: Seq[Pattern] =
      Iterator.iterate(p, 4)(_.reverse.transpose.map(_.mkString)).toSeq

    def perms: Seq[Pattern] =
      val rotate = p.rotated
      rotate ++ rotate.map(_.reverse)

    def step(rules: Rules): Pattern =
      val size = if p.size % 2 == 0 then 2 else 3
      p
        .map(_.grouped(size).toSeq)
        .grouped(size)
        .toSeq
        .flatMap(_.transpose.map(rules).transpose.map(_.mkString))

  val start: Pattern = Seq(".#.", "..#", "###")

object Parsing:
  import DataDefs.*

  def parseParts(line: String): Pattern = line.split("/").toSeq

  def parseLine(line: String): Seq[(Pattern, Pattern)] = line match
    case s"$pat => $rep" =>
      val pattern = parseParts(pat)
      val replace = parseParts(rep)
      pattern.perms.map(_ -> replace)

  def parse(lines: Seq[String]): Rules = lines.flatMap(parseLine).toMap

object Solving:
  import DataDefs.*

  def solve(lines: Seq[String])(iters: Int): Int =
    val rules = Parsing.parse(lines)
    Iterator
      .iterate(start)(_.step(rules))
      .drop(iters)
      .next()
      .map(_.count(_ == '#'))
      .sum

object Test:
  lazy val file  = os.pwd / "2017" / "21" / "21.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve(lines)(2)
// Test.res1 // part 1: 12

object Main:
  lazy val file  = os.pwd / "2017" / "21" / "21.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve(lines)(5)
  lazy val res2  = Solving.solve(lines)(18)
// Main.res1 // part 1: 133
// Main.res2 // part 2: 2221990
