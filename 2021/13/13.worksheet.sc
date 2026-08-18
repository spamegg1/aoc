object DataDefs:
  type Points = Set[(Int, Int)]
  type Folds  = Seq[(Char, Int)]

object Parsing:
  import DataDefs.*

  def parseFolds(lines: Seq[String]): Folds = lines
    .map: line =>
      line match
        case s"fold along $axis=$num" => (axis.head, num.toInt)

  def parsePoints(lines: Seq[String]): Points = lines
    .map: line =>
      line match
        case s"$n1,$n2" => (n1.toInt, n2.toInt)
    .toSet

object Solving:
  import DataDefs.*
  def origami(points: Points, folds: Folds): Points =
    folds.foldLeft(points):
      case (points, ('x', line)) =>
        points.map((x, y) => (if x < line then x else 2 * line - x, y))
      case (points, ('y', line)) =>
        points.map((x, y) => (x, if y < line then y else 2 * line - y))
      case _ => Set()

  def solve1(points: Points, folds: Folds) = origami(points, folds.take(1)).size
  def solve2(points: Points, folds: Folds) =
    val code            = origami(points, folds)
    val (width, height) = (code.map(_._1).max, code.map(_._2).max)
    (0 to height)
      .map: y =>
        (0 to width)
          .map: x =>
            if code.contains((x, y)) then "#" else "."
          .mkString
      .mkString("\n")

object Test:
  val file1  = os.pwd / "2021" / "13" / "13.test.input.txt"
  val file2  = os.pwd / "2021" / "13" / "13.test.folds.txt"
  val points = Parsing.parsePoints(os.read.lines(file1))
  val folds  = Parsing.parseFolds(os.read.lines(file2))
  val res1   = Solving.solve1(points, folds)
  val res2   = Solving.solve2(points, folds)
// Test.res1 // part 1: 17
// Test.res2 // part 2:

object Main:
  val file1  = os.pwd / "2021" / "13" / "13.input.txt"
  val file2  = os.pwd / "2021" / "13" / "13.folds.txt"
  val points = Parsing.parsePoints(os.read.lines(file1))
  val folds  = Parsing.parseFolds(os.read.lines(file2))
  val res1   = Solving.solve1(points, folds)
  val res2   = Solving.solve2(points, folds)
// Main.res1 // part 1: 810
// Main.res2 // part 2: HLBUBGFR
// #..#.#....###..#..#.###...##..####.###.
// #..#.#....#..#.#..#.#..#.#..#.#....#..#
// ####.#....###..#..#.###..#....###..#..#
// #..#.#....#..#.#..#.#..#.#.##.#....###.
// #..#.#....#..#.#..#.#..#.#..#.#....#.#.
// #..#.####.###...##..###...###.#....#..#
