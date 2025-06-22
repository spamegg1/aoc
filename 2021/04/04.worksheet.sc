object DataDefs:
  case class Number(num: Int, var marked: Boolean = false)

  case class Board(id: Int, lines: Seq[Seq[Number]]):
    def mark(draw: Int): Unit =
      for
        row <- lines
        col <- row
        if col.num == draw
      do col.marked = true
    def rowHasWon(row: Int): Boolean = lines(row).forall(_.marked)
    def colHasWon(col: Int): Boolean = (for row <- lines yield row(col)).forall(_.marked)
    def hasWon: Boolean = (0 until lines.size).exists(n => rowHasWon(n) || colHasWon(n))
    def score(draw: Int): Int = lines.flatten
      .filter(!_.marked)
      .map(_.num)
      .sum * draw

object Parsing:
  import DataDefs.*

  private def parseLine(line: String): Seq[Number] = line
    .split(" ")
    .filter(_.nonEmpty)
    .map(num => Number(num.toInt))
    .toSeq

  private def parseBoard(id: Int, lines: Seq[String]) = Board(id, lines.map(parseLine))
  private def parseBoards(id: Int, lines: Seq[String]): Seq[Board] =
    if lines.isEmpty then Seq()
    else parseBoard(id, lines.take(5)) +: parseBoards(id + 1, lines.drop(6))

  def parse(lines: Seq[String]) =
    val draws  = lines.head.split(",").map(_.toInt).toSeq
    val boards = parseBoards(1, lines.tail.tail)
    (draws, boards)

object Solving:
  import DataDefs.*, util.boundary, boundary.break

  def solve1(lines: Seq[String]) =
    val (draws, boards) = Parsing.parse(lines)
    boundary:
      for draw <- draws do
        boards.map(_.mark(draw))
        if boards.exists(_.hasWon) then break(boards.find(_.hasWon).get.score(draw))
    0

  def solve2(lines: Seq[String]) =
    val (draws, boards) = Parsing.parse(lines)
    var winners         = List[(Int, Int)]()
    for draw <- draws do
      boards.map(_.mark(draw))
      boards.foreach: board =>
        if board.hasWon && !winners.exists((id, _) => id == board.id) then
          winners = (board.id, board.score(draw)) :: winners
    winners.head

object Test:
  val file  = os.pwd / "2021" / "04" / "04.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 4512
// Test.res2 // part 2: 1924

object Main:
  val file  = os.pwd / "2021" / "04" / "04.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 74320
// Main.res2 // part 2: 17884
