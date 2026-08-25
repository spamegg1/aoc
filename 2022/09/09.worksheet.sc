object DataDefs:
  enum Dir:
    case Left, Right, Up, Down
  import Dir.*

  extension (s: String)
    def toDir: Dir = s match
      case "L" => Left
      case "R" => Right
      case "U" => Up
      case "D" => Down

  case class Move(dir: Dir, size: Int)

  case class Pos(x: Int, y: Int):
    def oneStep(dir: Dir): Pos = dir match
      case Left  => Pos(x - 1, y)
      case Right => Pos(x + 1, y)
      case Up    => Pos(x, y + 1)
      case Down  => Pos(x, y - 1)

    // .......    .......  ..T.T..    .......  ...T...    .......  .T...T.    .......
    // .T...T.    .......  .......    ...T...  .......    ...T...  .......    ..T.T..
    // ...H... -> ..THT..  ...H... -> ...H...  .T.H.T. -> ..THT..  ...H... -> ...H...
    // .T...T.    .......  .......    ...T...  .......    ...T...  .......    ..T.T..
    // .......    .......  ..T.T..    .......  ...T...    .......  .T...T.    .......
    def catchUpTo(head: Pos): Pos =
      if y == head.y && x == head.x - 2 then Pos(x + 1, y)
      else if y == head.y && x == head.x + 2 then Pos(x - 1, y)
      else if x == head.x && y == head.y - 2 then Pos(x, y + 1)
      else if x == head.x && y == head.y + 2 then Pos(x, y - 1)
      else if math.abs(y - head.y) == 1 && x == head.x - 2 then Pos(head.x - 1, head.y)
      else if math.abs(y - head.y) == 1 && x == head.x + 2 then Pos(head.x + 1, head.y)
      else if math.abs(x - head.x) == 1 && y == head.y - 2 then Pos(head.x, head.y - 1)
      else if math.abs(x - head.x) == 1 && y == head.y + 2 then Pos(head.x, head.y + 1)
      else if y - head.y == 2 && x == head.x - 2 then Pos(head.x - 1, head.y + 1)
      else if y - head.y == 2 && x == head.x + 2 then Pos(head.x + 1, head.y + 1)
      else if head.y - y == 2 && x == head.x - 2 then Pos(head.x - 1, head.y - 1)
      else if head.y - y == 2 && x == head.x + 2 then Pos(head.x + 1, head.y - 1)
      else this

  type State = Seq[Pos]

  case class Rope(state: State, visited: Map[Pos, Int]):
    private def oneStep(dir: Dir): Rope =
      val (head, rest) = (state.head, state.tail)
      val newHead      = head.oneStep(dir)
      val newState = rest.foldLeft[State](Seq(newHead)): (initial, pos) =>
        initial :+ pos.catchUpTo(initial.last)
      val newTail    = newState.last
      val newCount   = visited.getOrElse(newTail, 0) + 1
      val newVisited = visited.updated(newTail, newCount)
      Rope(newState, newVisited)

    private def applyMove(move: Move): Rope =
      (0 until move.size).foldLeft[Rope](this)((state, _) => state.oneStep(move.dir))

    def applyMoves(moves: Seq[Move]): Rope =
      moves.foldLeft[Rope](this)((state, move) => state.applyMove(move))

object Parsing:
  import DataDefs.*, Dir.*

  private def parseLine(line: String): Move = line match
    case s"$dir $size" => Move(dir.toDir, size.toInt)

  def parse(lines: Seq[String]): Seq[Move] = lines map parseLine

object Solving:
  import DataDefs.*

  private def rope(length: Int) = Rope(Seq.fill(length)(Pos(0, 0)), Map[Pos, Int]())

  def solve(lines: Seq[String])(length: Int) = rope(length)
    .applyMoves(Parsing.parse(lines))
    .visited
    .keys
    .size

object Test:
  val file1  = os.pwd / "2022" / "09" / "09.test.input.1.txt"
  val file2  = os.pwd / "2022" / "09" / "09.test.input.2.txt"
  val lines1 = os.read.lines(file1)
  val lines2 = os.read.lines(file2)
  val res1   = Solving.solve(lines1)(2)
  val res2   = Solving.solve(lines2)(10)
// Test.res1 // part 1: 13
// Test.res2 // part 2: 36

object Main:
  val file  = os.pwd / "2022" / "09" / "09.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve(lines)(2)
  val res2  = Solving.solve(lines)(10)
// Main.res1 // part 1: 6367
// Main.res2 // part 2: 2536
