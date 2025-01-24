object Solving:
  val regex = """(")|[a-z]|(\\")|(\\x([0-9]|[a-f]){2})|(\\\\)""".r

  def replace(old: String): String = old match
    case """""""        => """\""""
    case """\""""       => """\\\""""
    case """\\"""       => """\\\\"""
    case s"""\\x$hex""" => s"""\\\\x$hex"""
    case _              => old

  def solve1(lines: Seq[String]) =
    (lines.map(_.length).sum, lines.map(regex.findAllIn(_).length - 2).sum)

  def solve2(lines: Seq[String]) =
    val newSizes = lines.map: line =>
      regex
        .findAllIn(line)
        .map(replace)
        .mkString
        .length + 2
    (newSizes.sum, lines.map(_.length).sum)

object Test:
  lazy val file  = os.pwd / "2015" / "08" / "08.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: (23, 11) = 12
// Test.res2 // part 2: (42, 23) = 19

object Main:
  lazy val file  = os.pwd / "2015" / "08" / "08.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: (6195, 4845) = 1350
// Main.res2 // part 2: (8280, 6195) = 2085
