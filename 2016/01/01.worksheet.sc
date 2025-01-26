object DataDefs:
  enum Direction:
    case N, S, E, W
  import Direction.*

  enum Move:
    case Left(blocks: Int)
    case Right(blocks: Int)
  import Move.*

  type Pos  = (Int, Int)
  type Path = Seq[Pos]
  extension (p: Pos) def distance = math.abs(p._1) + math.abs(p._2)

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

  def parseMoves(line: String): List[Move] = line
    .split(", ")
    .map(parseMove)
    .toList

object Solving:
  import DataDefs.*
  val startPos   = Position(0, 0, Direction.N)
  val startCoord = (0, 0)

  def makeMoves(start: Position)(moves: List[Move]): Position =
    moves.foldLeft(start)((position, move) => position.go(move))

  def solve1(line: String): Int =
    makeMoves(startPos)(Parsing.parseMoves(line)).distance

  @annotation.tailrec
  def travel(current: Position)(moves: List[Move])(visited: Path): Pos =
    moves match
      case head :: next =>
        val path         = current.path(head)
        val intersection = visited.intersect(path)
        if intersection.nonEmpty then intersection.head
        else travel(current.go(head))(next)(path ++ visited)
      case Nil => (current.x, current.y)

  def solve2(line: String) =
    travel(startPos)(Parsing.parseMoves(line))(Seq(startCoord)).distance

object Test:
  lazy val inps = Seq("R2, L3", "R2, R2, R2", "R5, L5, R5, R3")
  lazy val res1 = inps map Solving.solve1
  lazy val res2 = Solving.solve2("R8, R4, R4, R8")
// Test.res1 // part 1: 5, 2, 12
// Test.res2 // part 2: 4

object Main:
  lazy val file = os.pwd / "2016" / "01" / "01.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 298
// Main.res2 // part 2: 158 (position is -22, 136)
