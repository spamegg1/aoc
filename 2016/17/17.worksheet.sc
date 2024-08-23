/*
--- Day 17: Two Steps Forward ---
You're trying to access a secure vault protected by a 4x4 grid of
small rooms connected by doors. You start in the top-left room (marked S),
and you can access the vault (marked V) once you reach the bottom-right room:

#########
#S| | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | | #
#-#-#-#-#
# | | |
####### V

Fixed walls are marked with #, and doors are marked with - or |.

The doors in your current room are either open or closed (and locked)
based on the hexadecimal MD5 hash of a passcode (your puzzle input)
followed by a sequence of uppercase characters representing the path
you have taken so far (U for up, D for down, L for left, and R for right).

Only the first four characters of the hash are used; they represent, respectively,
the doors up, down, left, and right from your current position.
Any b, c, d, e, or f means that the corresponding door is open; any other character
(any number or a) means that the corresponding door is closed and locked.

To access the vault, all you need to do is reach the bottom-right room;
reaching this room opens the vault and all doors in the maze.

For example, suppose the passcode is hijkl.
Initially, you have taken no steps, and so your path is empty:
you simply find the MD5 hash of hijkl alone.
The first four characters of this hash are ced9,
  which indicate that up is open (c), down is open (e),
  left is open (d), and right is closed and locked (9).
Because you start in the top-left corner,
there are no "up" or "left" doors to be open, so your only choice is down.

Next, having gone only one step (down, or D), you find the hash of hijklD.
This produces f2bc, which indicates that you can go back up,
  left (but that's a wall), or right.
Going right means hashing hijklDR to get 5745 - all doors closed and locked.
However, going up instead is worthwhile:
even though it returns you to the room you started in,
your path would then be DU, opening a different set of doors.

After going DU (and then hashing hijklDU to get 528e),
only the right door is open;
after going DUR, all doors lock.
(Fortunately, your actual passcode is not hijkl).

Passcodes actually used by Easter Bunny Vault Security do allow
access to the vault if you know the right path. For example: If your passcode
  were ihgpwlah, the shortest path would be DDRRRD.
  With kglvqrro, the shortest path would be DDUDRLRRUDRD.
  With ulqzkmiv, the shortest path would be DRURDRUDDLLDLUURRDULRLDUUDDDRR.

Given your vault's passcode, what is the shortest path
(the actual path, not just the length) to reach the vault?

--- Part Two ---
You're curious how robust this security solution really is,
and so you decide to find longer and longer paths which still
provide access to the vault. You remember that paths always
end the first time they reach the bottom-right room
(that is, they can never pass through it, only end in it).

For example: If your passcode
  were ihgpwlah, the longest path would take 370 steps.
  With kglvqrro, the longest path would be 492 steps long.
  With ulqzkmiv, the longest path would be 830 steps long.

What is the length of the longest path that reaches the vault?
 */
import org.apache.commons.codec.digest.DigestUtils.md5Hex
import scala.util.boundary, boundary.break

object DataDefs:
  enum Move:
    case U, D, L, R
    def inBounds(row: Int, col: Int) = this match
      case U => 0 < row
      case D => row < 3
      case L => 0 < col
      case R => col < 3
  import Move.*

  extension (c: Char)
    def isOpen = c match
      case 'b' | 'c' | 'd' | 'e' | 'f' => true
      case _                           => false

  case class Pos(row: Int, col: Int, code: String, path: String):
    def isVault = row == 3 && col == 3
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
    var pos = Pos(0, 0, code, "")
    val queue = collection.mutable.Queue[Pos](pos)
    var vaults = List[Pos]()
    while queue.nonEmpty do
      pos = queue.dequeue()
      if pos.isVault then vaults ::= pos
      else queue.enqueueAll(pos.makeAllMoves)
    vaults

  def solve1(code: String) = solve(code).minBy(_.path.length).path
  def solve2(code: String) = solve(code).maxBy(_.path.length).path.length

object Testing:
  lazy val result11 = Solving.solve1("ihgpwlah")
  lazy val result12 = Solving.solve1("kglvqrro")
  lazy val result13 = Solving.solve1("ulqzkmiv")
  lazy val result21 = Solving.solve2("ihgpwlah")
  lazy val result22 = Solving.solve2("kglvqrro")
  lazy val result23 = Solving.solve2("ulqzkmiv")
// Testing.result11 // part 1: DDRRRD
// Testing.result12 // part 1: DDUDRLRRUDRD
// Testing.result13 // part 1: DRURDRUDDLLDLUURRDULRLDUUDDDRR
// Testing.result21 // part 2: 370
// Testing.result22 // part 2: 492
// Testing.result23 // part 2: 830

object Main:
  lazy val result1 = Solving.solve1("veumntbg")
  lazy val result2 = Solving.solve2("veumntbg")
// Main.result1 // part 1: DDRRULRDRD
// Main.result2 // part 2: 536
