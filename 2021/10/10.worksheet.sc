object DataDefs:
  enum Bracket:
    case AngleOpen,
      AngleClose,
      CornerOpen,
      CornerClose,
      CurlyOpen,
      CurlyClose,
      ParenOpen,
      ParenClose

    lazy val isOpen = this match
      case AngleOpen | CornerOpen | CurlyOpen | ParenOpen => true
      case _                                              => false

    lazy val pair = this match
      case AngleOpen   => AngleClose
      case CornerOpen  => CornerClose
      case CurlyOpen   => CurlyClose
      case ParenOpen   => ParenClose
      case AngleClose  => AngleOpen
      case CornerClose => CornerOpen
      case CurlyClose  => CurlyOpen
      case ParenClose  => ParenOpen

    lazy val points1 = this match
      case AngleClose  => 25137L
      case CornerClose => 57L
      case CurlyClose  => 1197L
      case ParenClose  => 3L
      case _           => 0L

    lazy val points2 = this match
      case AngleClose  => 4L
      case CornerClose => 2L
      case CurlyClose  => 3L
      case ParenClose  => 1L
      case _           => 0L

    def matches(that: Bracket) = pair == that
  import Bracket.*

  extension (c: Char)
    def toBracket = c match
      case '<' => AngleOpen
      case '>' => AngleClose
      case '[' => CornerOpen
      case ']' => CornerClose
      case '{' => CurlyOpen
      case '}' => CurlyClose
      case '(' => ParenOpen
      case ')' => ParenClose

  enum Result:
    case Balanced
    case Incomplete(opens: List[Bracket])
    case Corrupted(b: Bracket)

    lazy val isCorrupted = this match
      case Corrupted(b) => true
      case _            => false

    lazy val points = this match
      case Corrupted(b) => b.points1
      case Incomplete(opens) =>
        opens.foldLeft(0L): (total, openBracket) =>
          total * 5L + openBracket.pair.points2
      case Balanced => 0L

  import Result.*

  case class Brackets(brackets: List[Bracket]):
    @annotation.tailrec
    private def helper(bracks: List[Bracket], open: List[Bracket]): Result = bracks match
      case Nil => if open.nonEmpty then Incomplete(open) else Balanced
      case bracket :: bs =>
        open match
          case openBracket :: opens =>
            if openBracket.matches(bracket) then helper(bs, opens)
            else if bracket.isOpen then helper(bs, bracket :: open)
            else Corrupted(bracket)
          case Nil =>
            if bracket.isOpen then helper(bs, bracket :: open) else Corrupted(bracket)

    lazy val res: Result = helper(brackets, Nil)
    lazy val points      = res.points

object Parsing:
  import DataDefs.*
  def parseLine(line: String)   = Brackets(line.toList.map(_.toBracket))
  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .view
    .map(_.res)
    .filter(_.isCorrupted)
    .map(_.points)
    .sum

  def solve2(lines: Seq[String]) =
    val points2 = Parsing
      .parse(lines)
      .view
      .map(_.res)
      .filterNot(_.isCorrupted)
      .map(_.points)
      .toSeq
      .sorted

    points2.drop(points2.length / 2).head

object Test:
  val file  = os.pwd / "2021" / "10" / "10.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 26397
// Test.res2 // part 2: 288957

object Main:
  val file  = os.pwd / "2021" / "10" / "10.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 367059
// Main.res2 // part 2: 1952146692
