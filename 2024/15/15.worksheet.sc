/*
--- Day 15: Warehouse Woes ---
You appear back inside your own mini submarine!
Each Historian drives their mini submarine in a different direction;
maybe the Chief has his own submarine down here somewhere as well?

You look up to see a vast school of lanternfish swimming past you.
On closer inspection, they seem quite anxious, so you drive your
mini submarine over to see if you can help.

Because lanternfish populations grow rapidly, they need a lot of food,
and that food needs to be stored somewhere. That's why these lanternfish
have built elaborate warehouse complexes operated by robots!

These lanternfish seem so anxious because they have lost control of the
robot that operates one of their most important warehouses!
It is currently running amok, pushing around boxes in the warehouse with no
regard for lanternfish logistics or lanternfish inventory management strategies.

Right now, none of the lanternfish are brave enough to swim up to an
unpredictable robot so they could shut it off.
However, if you could anticipate the robot's movements,
maybe they could find a safe option.

The lanternfish already have a map of the warehouse and a list of movements
the robot will attempt to make (your puzzle input).
The problem is that the movements will sometimes fail as boxes are shifted around,
making the actual movements of the robot difficult to predict.

For example:

##########
#..O..O.O#
#......O.#
#.OO..O.O#
#..O@..O.#
#O#..O...#
#O..O..O.#
#.OO.O.OO#
#....O...#
##########

<vv>^<v^>v>^vv^v>v<>v^v<v<^vv<<<^><<><>>v<vvv<>^v^>^<<<><<v<<<v^vv^v>^
vvv<<^>^v^^><<>>><>^<<><^vv^^<>vvv<>><^^v>^>vv<>v<<<<v<^v>^<^^>>>^<v<v
><>vv>v^v^<>><>>>><^^>vv>v<^^^>>v^v^<^^>v^^>v^<^v>v<>>v^v^<v>v^^<^^vv<
<<v<^>>^^^^>>>v^<>vvv^><v<<<>^^^vv^<vvv>^>v<^^^^v<>^>vvvv><>>v^<<^^^^^
^><^><>>><>^^<<^^v>>><^<v>^<vv>>v>>>^v><>^v><<<<v>>v<v<v>vvv>^<><<>^><
^>><>^v<><^vvv<^^<><v<<<<<><^v<<<><<<^^<v<^^^><^>>^<v^><<<^>>^v<v^v<v^
>^>>^v>vv>^<<^v<>><<><<v<<v><>v<^vv<<<>^^v^>^^>>><<^v>>v^v><^^>>^<>vv^
<><^^>^^^<><vvvvv^v<v<<>^v<v>v<<^><<><<><<<^^<<<^<<>><<><^^^>^^<>^>v<>
^^>vv<^v^v<vv>^<><v<^v>^^^>>>^^vvv^>vvv<>>>^<^>>>>>^<<^v>^vvv<>^<><<v>
v^^>>><<^^<>>^v^<v^vv<>v^<<>^<^v^v><^<<<><<^<v><v<>vv>>v><v^<vv<>v^<<^

As the robot (@) attempts to move, if there are any boxes (O) in the way,
the robot will also attempt to push those boxes.
However, if this action would cause the robot or a box to move into a wall (#),
nothing moves instead, including the robot.
The initial positions of these are shown on the map at the top of
the document the lanternfish gave you.

The rest of the document describes the moves
(^ for up, v for down, < for left, > for right)
that the robot will attempt to make, in order.
(The moves form a single giant sequence;
they are broken into multiple lines just to make copy-pasting easier.
Newlines within the move sequence should be ignored.)

Here is a smaller example to get started:

########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

<^^>>>vv<v>>v<<

Were the robot to attempt the given sequence of moves,
it would push around the boxes as follows:

Initial state:
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move <:
########
#..O.O.#
##@.O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move ^:
########
#.@O.O.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move ^:
########
#.@O.O.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#..@OO.#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#...@OO#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move >:
########
#...@OO#
##..O..#
#...O..#
#.#.O..#
#...O..#
#......#
########

Move v:
########
#....OO#
##..@..#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##..@..#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.@...#
#...O..#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##.....#
#..@O..#
#.#.O..#
#...O..#
#...O..#
########

Move >:
########
#....OO#
##.....#
#...@O.#
#.#.O..#
#...O..#
#...O..#
########

Move >:
########
#....OO#
##.....#
#....@O#
#.#.O..#
#...O..#
#...O..#
########

Move v:
########
#....OO#
##.....#
#.....O#
#.#.O@.#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########

Move <:
########
#....OO#
##.....#
#.....O#
#.#O@..#
#...O..#
#...O..#
########

The larger example has many more moves;
after the robot has finished those moves, the warehouse would look like this:

##########
#.O.O.OOO#
#........#
#OO......#
#OO@.....#
#O#.....O#
#O.....OO#
#O.....OO#
#OO....OO#
##########

The lanternfish use their own custom Goods Positioning System
(GPS for short) to track the locations of the boxes.
The GPS coordinate of a box is equal to 100 times its distance
from the top edge of the map plus its distance from the left edge of the map.
(This process does not stop at wall tiles; measure all the way to the edges of the map.)

So, the box shown below has a distance of 1 from the top edge of the map and
4 from the left edge of the map, resulting in a GPS coordinate of 100 * 1 + 4 = 104.

#######
#...O..
#......

The lanternfish would like to know the sum of all boxes'
GPS coordinates after the robot finishes moving.
In the larger example, the sum of all boxes' GPS coordinates is 10092.
In the smaller example, the sum is 2028.

Predict the motion of the robot and boxes in the warehouse.
After the robot is finished moving, what is the sum of all boxes' GPS coordinates?

--- Part Two ---
The lanternfish use your information to find a safe moment to swim in and
turn off the malfunctioning robot! Just as they start preparing a festival
in your honor, reports start coming in that a second warehouse's robot is
also malfunctioning.

This warehouse's layout is surprisingly similar to the one you just helped.
There is one key difference: everything except the robot is twice as wide!
The robot's list of movements doesn't change.

To get the wider warehouse's map, start with your original map and,
for each tile, make the following changes:
  If the tile is #, the new map contains ## instead.
  If the tile is O, the new map contains [] instead.
  If the tile is ., the new map contains .. instead.
  If the tile is @, the new map contains @. instead.

This will produce a new warehouse map which is twice as wide and
with wide boxes that are represented by []. (The robot does not change size.)

The larger example from before would now look like this:

####################
##....[]....[]..[]##
##............[]..##
##..[][]....[]..[]##
##....[]@.....[]..##
##[]##....[]......##
##[]....[]....[]..##
##..[][]..[]..[][]##
##........[]......##
####################

Because boxes are now twice as wide but the robot is still the same size and speed,
boxes can be aligned such that they directly push two other boxes at once.
For example, consider this situation:

#######
#...#.#
#.....#
#..OO@#
#..O..#
#.....#
#######

<vv<<^^<<^^

After appropriately resizing this map,
the robot would push around these boxes as follows:

Initial state:
##############
##......##..##
##..........##
##....[][]@.##
##....[]....##
##..........##
##############

Move <:
##############
##......##..##
##..........##
##...[][]@..##
##....[]....##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[].@..##
##..........##
##############

Move v:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.......@..##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##......@...##
##############

Move <:
##############
##......##..##
##..........##
##...[][]...##
##....[]....##
##.....@....##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##....[]....##
##.....@....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##....@.....##
##..........##
##############

Move <:
##############
##......##..##
##...[][]...##
##....[]....##
##...@......##
##..........##
##############

Move ^:
##############
##......##..##
##...[][]...##
##...@[]....##
##..........##
##..........##
##############

Move ^:
##############
##...[].##..##
##...@.[]...##
##....[]....##
##..........##
##..........##
##############

This warehouse also uses GPS to locate the boxes.
For these larger boxes, distances are measured from the edge of the map
to the closest edge of the box in question.
So, the box shown below has a distance of 1 from the top edge of the map and
5 from the left edge of the map, resulting in a GPS coordinate of 100 * 1 + 5 = 105.

##########
##...[]...
##........

In the scaled-up version of the larger example from above,
after the robot has finished all of its moves, the warehouse would look like this:

####################
##[].......[].[][]##
##[]...........[].##
##[]........[][][]##
##[]......[]....[]##
##..##......[]....##
##..[]............##
##..@......[].[][]##
##......[][]..[]..##
####################

The sum of these boxes' GPS coordinates is 9021.

Predict the motion of the robot and boxes in this new, scaled-up warehouse.
What is the sum of all boxes' final GPS coordinates?
 */
object DataDefs:
  enum Dir:
    case N, S, E, W
  import Dir.*

  enum Space:
    case Wall, Box, Vacant
  import Space.*

  extension (c: Char)
    def toDir = c match
      case '^' => N
      case 'v' => S
      case '<' => W
      case '>' => E

    def toSpace = c match
      case '#' => Wall
      case 'O' => Box
      case '.' => Vacant

  type Pos = (row: Int, col: Int) // this is also Robot
  extension (p: Pos)
    def move(d: Dir): Pos = d match
      case N => (p.row - 1, p.col)
      case S => (p.row + 1, p.col)
      case E => (p.row, p.col + 1)
      case W => (p.row, p.col - 1)

  case class Warehouse(
      grid: Array[Array[Space]],
      var robot: Pos,
      var moves: List[Dir]
  )(using size: Int):
    def slice(pos: Pos, dir: Dir): Seq[(Pos, Space)] = dir match
      case N => for r <- pos.row to 0 by -1 yield ((r, pos.col), grid(r)(pos.col))
      case S => for r <- pos.row until size yield ((r, pos.col), grid(r)(pos.col))
      case E => for c <- pos.col until size yield ((pos.row, c), grid(pos.row)(c))
      case W => for c <- pos.col to 0 by -1 yield ((pos.row, c), grid(pos.row)(c))

    def boxes(pos: Pos, dir: Dir): Option[Pos] =
      val slic = slice(pos, dir) // assume grid(pos) == Box
      val rest = slic.dropWhile(_._2 == Box)
      if rest.head._2 == Vacant then Some(rest.head._1) else None

    def nextState: Unit = moves match
      case head :: next =>
        val newPos = robot.move(head)
        grid(newPos.row)(newPos.col) match
          case Wall => () // can't move, do nothing
          case Box => // complicated case
            boxes(newPos, head) match // endpoint of boxes in that direction
              case None => () // no room to push boxes, do nothing
              case Some(value) => // there is room to push boxes
                grid(newPos.row)(newPos.col) = Vacant // push box out of newPos
                grid(value.row)(value.col) = Box      // move boxes 1 unit in that dir
                robot = newPos                        // move robot to newPos
          case Vacant => robot = newPos
        moves = next
      case Nil => ()

    def gps: Int =
      (for
        row <- 0 until size
        col <- 0 until size
        if grid(row)(col) == Box
      yield 100 * row + col).sum

object Parsing:
  import DataDefs.*

  def parseMoves(lines: Seq[String]): List[Dir] = lines.toList.flatMap: line =>
    line.map(_.toDir).toList

  def parseWarehouse(lines: Seq[String]): Array[Array[Space]] = lines
    .map(line => line.map(_.toSpace).toArray)
    .toArray

object Solving:
  import DataDefs.*

  def solve1(boxes: Seq[String], moves: Seq[String], start: Pos)(using size: Int) =
    val warehouse = Warehouse(
      grid = Parsing.parseWarehouse(boxes),
      robot = start,
      moves = Parsing.parseMoves(moves)
    )
    while warehouse.moves.nonEmpty do warehouse.nextState
    warehouse.gps

  def solve2(boxes: Seq[String], moves: Seq[String], start: Pos)(using size: Int) = 0L

object Testing: // robot starts at 2,2 and 4,4
  lazy val boxes1   = os.read.lines(os.pwd / "2024" / "15" / "15.test.input.1.txt")
  lazy val moves1   = os.read.lines(os.pwd / "2024" / "15" / "15.test.input.2.txt")
  lazy val boxes2   = os.read.lines(os.pwd / "2024" / "15" / "15.test.input.3.txt")
  lazy val moves2   = os.read.lines(os.pwd / "2024" / "15" / "15.test.input.4.txt")
  val start1        = (2, 2)
  val start2        = (4, 4)
  lazy val result11 = Solving.solve1(boxes1, moves1, start1)(using 8)
  lazy val result12 = Solving.solve1(boxes2, moves2, start2)(using 10)
  lazy val result21 = Solving.solve2(boxes1, moves1, start1)(using 8)
  lazy val result22 = Solving.solve2(boxes2, moves2, start2)(using 10)
// Testing.result11 // part 1: 2028
// Testing.result12 // part 1: 10092
// Testing.result21 // part 2: 9021
// Testing.result22 // part 2: 9021

object Main: // robot starts at 24,24
  lazy val boxes   = os.read.lines(os.pwd / "2024" / "15" / "15.input.1.txt")
  lazy val moves   = os.read.lines(os.pwd / "2024" / "15" / "15.input.2.txt")
  val start        = (24, 24)
  given size: Int  = 50
  lazy val result1 = Solving.solve1(boxes, moves, start)
  lazy val result2 = Solving.solve2(boxes, moves, start)
// Main.result1 // part 1: 1478649
// Main.result2 // part 2:
