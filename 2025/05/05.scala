package aoc2025.day05

object DataDefs:
  type Range = (lo: Long, hi: Long)
  extension (r: Range) def size = r.hi - r.lo + 1

object Parsing:
  import DataDefs.*
  def parseRange(line: String): Range = line match
    case s"$lo-$hi" => (lo = lo.toLong, hi = hi.toLong)

  def parse(lines: String) =
    val Array(ranges, ingredients) = lines.split("\n\n").map(_.split("\n"))
    (ranges.map(parseRange).sortBy(_.lo), ingredients.map(_.toLong).sorted)

object Solving:
  import DataDefs.*

  def solve1(lines: String) =
    val (ranges, ingredients) = Parsing.parse(lines)
    ingredients.count: ingr =>
      ranges.exists: range =>
        range.lo <= ingr && ingr <= range.hi

  def solve2(lines: String) = Parsing
    .parse(lines)
    ._1
    .foldLeft(List.empty[Range]):
      case (Nil, range) => List(range)
      case (prev :: rest, range) =>
        if range.lo <= prev.hi + 1 then
          val newRange = (lo = prev.lo, hi = prev.hi.max(range.hi))
          newRange :: rest
        else range :: prev :: rest
    .foldLeft(0L)(_ + _.size)

object Test:
  val file  = os.pwd / "2025" / "05" / "05.test.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2025" / "05" / "05.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 3
  println(Test.res2) // part 2: 14
  println(Main.res1) // part 1: 681
  println(Main.res2) // part 2: 348820208020395
