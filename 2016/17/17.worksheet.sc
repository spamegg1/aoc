import org.apache.commons.codec.digest.DigestUtils.md5Hex
import scala.util.boundary, boundary.break

object DataDefs:
  val SIZE = 3

  enum Move:
    case U, D, L, R
    def inBounds(row: Int, col: Int) = this match
      case U => 0 < row
      case D => row < SIZE
      case L => 0 < col
      case R => col < SIZE
  import Move.*

  extension (c: Char)
    def isOpen = c match
      case 'b' | 'c' | 'd' | 'e' | 'f' => true
      case _                           => false

  case class Pos(row: Int, col: Int, code: String, path: String):
    def isVault      = row == 3 && col == 3
    def makeAllMoves = possibleMoves.map(makeMove)

    def possibleMoves: Seq[Move] = md5Hex(code + path).view
      .take(4)
      .zip(Move.values)
      .filter(_._1.isOpen)
      .filter(_._2.inBounds(row, col))
      .map(_._2)
      .toSeq

    def makeMove(move: Move): Pos = move match // assume move is possible
      case U => Pos(row - 1, col, code, path + "U")
      case D => Pos(row + 1, col, code, path + "D")
      case L => Pos(row, col - 1, code, path + "L")
      case R => Pos(row, col + 1, code, path + "R")

object Solving:
  import DataDefs.*

  def solve(code: String) =
    var pos    = Pos(0, 0, code, "")
    val queue  = collection.mutable.Queue[Pos](pos)
    var vaults = List[Pos]()
    while queue.nonEmpty do
      pos = queue.dequeue()
      if pos.isVault then vaults ::= pos
      else queue.enqueueAll(pos.makeAllMoves)
    vaults

  def solve1(code: String) = solve(code).minBy(_.path.length).path
  def solve2(code: String) = solve(code).maxBy(_.path.length).path.length

object Test:
  lazy val res11 = Solving.solve1("ihgpwlah")
  lazy val res12 = Solving.solve1("kglvqrro")
  lazy val res13 = Solving.solve1("ulqzkmiv")
  lazy val res21 = Solving.solve2("ihgpwlah")
  lazy val res22 = Solving.solve2("kglvqrro")
  lazy val res23 = Solving.solve2("ulqzkmiv")
// Test.res11 // part 1: DDRRRD
// Test.res12 // part 1: DDUDRLRRUDRD
// Test.res13 // part 1: DRURDRUDDLLDLUURRDULRLDUUDDDRR
// Test.res21 // part 2: 370
// Test.res22 // part 2: 492
// Test.res23 // part 2: 830

object Main:
  lazy val res1 = Solving.solve1("veumntbg")
  lazy val res2 = Solving.solve2("veumntbg")
// Main.res1 // part 1: DDRRULRDRD
// Main.res2 // part 2: 536
