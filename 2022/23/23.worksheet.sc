object DataDefs:
  case class Pos(x: Int, y: Int):
    def +(other: Pos): Pos = Pos(x + other.x, y + other.y)

  val North    = Pos(0, -1)
  val NW       = Pos(-1, -1)
  val NE       = Pos(1, -1)
  val South    = Pos(0, 1)
  val SW       = Pos(-1, 1)
  val SE       = Pos(1, 1)
  val West     = Pos(-1, 0)
  val East     = Pos(1, 0)
  val Order    = Seq(North, South, West, East)
  val Adjacent = Seq(NW, North, NE, West, East, SW, South, SE)

  case class State(elves: Set[Pos], moves: Seq[Pos], stuck: Boolean):
    def step: State =
      val proposals = elves.map(elf => elf -> Solving.propose(elves, moves, elf))
      val occurrences = proposals.toSeq
        .flatMap(_._2)
        .groupMapReduce(identity)(_ => 1)(_ + _)
      val next = proposals.map: (elf, proposal) =>
        proposal
          .map: move =>
            if occurrences(move) == 1 then move else elf
          .getOrElse(elf)
      State(next, moves.tail :+ moves.head, elves == next)
    end step

object Parsing:
  import DataDefs.*

  def parse(lines: Seq[String]): Set[Pos] =
    (for
      y <- lines.indices
      x <- lines.head.indices
      if lines(y)(x) != '.'
    yield Pos(x, y)).toSet

object Solving:
  import DataDefs.*

  def propose(elves: Set[Pos], moves: Seq[Pos], elf: Pos): Option[Pos] =
    val checks = Adjacent
      .map(_ + elf)
      .map(elves.contains)
    val Seq(nw, n, ne, w, e, sw, s, se) = checks

    if checks.exists(identity) then
      moves
        .find:
          case `North` => !(nw || n || ne)
          case `South` => !(sw || s || se)
          case `West`  => !(nw || w || sw)
          case `East`  => !(ne || e || se)
          case _       => false
        .map(_ + elf)
    else None
  end propose

  def solve1(lines: Seq[String]) =
    val start = State(Parsing.parse(lines), Order, false)
    val elves = Iterator
      .iterate(start)(_.step)
      .drop(10)
      .next()
      .elves
    val (minX, maxX) = (elves.map(_.x).min, elves.map(_.x).max)
    val (minY, maxY) = (elves.map(_.y).min, elves.map(_.y).max)
    (for
      x <- minX to maxX
      y <- minY to maxY
    yield Pos(x, y))
      .filterNot(elves.contains)
      .size
  end solve1

  def solve2(lines: Seq[String]) =
    val start = State(Parsing.parse(lines), Order, false)
    Iterator
      .iterate(start)(_.step)
      .indexWhere(_.stuck == true)

object Test:
  val file  = os.pwd / "2022" / "23" / "23.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 110
// Test.res2 // part 2: 20

object Main:
  val file  = os.pwd / "2022" / "23" / "23.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 4158
// Main.res2 // part 2: 1014
