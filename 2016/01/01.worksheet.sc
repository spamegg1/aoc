/*
--- Day 1: No Time for a Taxicab ---
Santa's sleigh uses a very high-precision clock to guide its movements, and the
clock's oscillator is regulated by stars. Unfortunately, the stars have been
stolen... by the Easter Bunny. To save Christmas, Santa needs you to retrieve
all fifty stars by December 25th.

Collect stars by solving puzzles. Two puzzles will be made available on each day
in the Advent calendar; the second puzzle is unlocked when you complete the
first. Each puzzle grants one star. Good luck!

You're airdropped near Easter Bunny Headquarters in a city somewhere. "Near",
unfortunately, is as close as you can get - the instructions on the Easter Bunny
Recruiting Document the Elves intercepted start here, and nobody had time to
work them out further.

The Document indicates that you should start at the given coordinates (where you
just landed) and face North. Then, follow the provided sequence: either turn
left (L) or right (R) 90 degrees, then walk forward the given number of blocks,
ending at a new intersection.

There's no time to follow such ridiculous instructions on foot, though, so you
take a moment and work out the destination. Given that you can only walk on the
street grid of the city, how far is the shortest path to the destination?

For example:
Following R2, L3 leaves you 2 blocks East and 3 blocks North, or 5 blocks away.
R2, R2, R2 leaves you 2 blocks due South of your starting position, which is 2
blocks away.
R5, L5, R5, R3 leaves you 12 blocks away.

How many blocks away is Easter Bunny HQ?

--- Part Two ---
Then, you notice the instructions continue on the back of the Recruiting
Document. Easter Bunny HQ is actually at the first location you visit twice.

For example, if your instructions are R8, R4, R4, R8, the first location you
visit twice is 4 blocks away, due East.

How many blocks away is the first location you visit twice?
 */
object DataDefs:
  enum Direction:
    case N, S, E, W
  import Direction.*

  enum Move:
    case Left(blocks: Int)
    case Right(blocks: Int)
  import Move.*

  type Point = (Int, Int)
  type Path = Seq[Point]
  extension (p: Point) def distance = math.abs(p._1) + math.abs(p._2)

  case class Position(x: Int, y: Int, facing: Direction):
    def go(move: Move): Position = (move, facing) match
      case (Left(blocks), N)  => Position(x - blocks, y, W)
      case (Left(blocks), S)  => Position(x + blocks, y, E)
      case (Left(blocks), E)  => Position(x, y + blocks, N)
      case (Left(blocks), W)  => Position(x, y - blocks, S)
      case (Right(blocks), N) => Position(x + blocks, y, E)
      case (Right(blocks), S) => Position(x - blocks, y, W)
      case (Right(blocks), E) => Position(x, y - blocks, S)
      case (Right(blocks), W) => Position(x, y + blocks, N)

    def distance = math.abs(x) + math.abs(y)

    def path(move: Move): Path =
      val end = go(move)
      (move, facing) match
        case (Left(blocks), N)  => for xc <- end.x until x yield (xc, y)
        case (Left(blocks), S)  => for xc <- x + 1 to end.x yield (xc, y)
        case (Left(blocks), E)  => for yc <- y + 1 to end.y yield (x, yc)
        case (Left(blocks), W)  => for yc <- end.y until y yield (x, yc)
        case (Right(blocks), N) => for xc <- x + 1 to end.x yield (xc, y)
        case (Right(blocks), S) => for xc <- end.x until x yield (xc, y)
        case (Right(blocks), E) => for yc <- end.y until y yield (x, yc)
        case (Right(blocks), W) => for yc <- y + 1 to end.y yield (x, yc)

object Parsing:
  import DataDefs.*, Move.*

  def parseMove(move: String): Move = move match
    case s"L$i" => Left(i.toInt)
    case s"R$i" => Right(i.toInt)

  def parseMoves(line: String): List[Move] = line.split(", ").map(parseMove(_)).toList

object Solving:
  import DataDefs.*
  val startPos = Position(0, 0, Direction.N)
  val startCoord = (0, 0)

  def makeMoves(start: Position)(moves: List[Move]): Position =
    moves.foldLeft(start)((position, move) => position.go(move))

  def solve1(line: String): Int =
    makeMoves(startPos)(Parsing.parseMoves(line)).distance

  @annotation.tailrec
  def travel(current: Position)(moves: List[Move])(visited: Path): Point =
    moves match
      case head :: next =>
        val path = current.path(head)
        val intersection = visited.intersect(path)
        if intersection.nonEmpty then intersection.head
        else travel(current.go(head))(next)(path ++ visited)
      case Nil => (current.x, current.y)

  def solve2(line: String) =
    travel(startPos)(Parsing.parseMoves(line))(Seq(startCoord)).distance

object Testing:
  lazy val input1 = "R2, L3"
  lazy val input2 = "R2, R2, R2"
  lazy val input3 = "R5, L5, R5, R3"
  lazy val input4 = "R8, R4, R4, R8"
  lazy val result1 = Solving.solve1(input1)
  lazy val result2 = Solving.solve1(input2)
  lazy val result3 = Solving.solve1(input3)
  lazy val result4 = Solving.solve2(input4)
Testing.result1 // part 1: 5
Testing.result2 // part 1: 2
Testing.result3 // part 1: 12
Testing.result4 // part 2: 4 (position is 4,0)

object Main:
  lazy val line = os.read.lines(os.pwd / "01.input.txt")(0)
  lazy val result1 = Solving.solve1(line)
  lazy val result2 = Solving.solve2(line)
Main.result1 // part 1: 298
Main.result2 // part 2: 158 (position is -22, 136)
