/*
--- Day 21: Step Counter ---
You manage to catch the airship right as it's dropping someone else off on their
all-expenses-paid trip to Desert Island! It even helpfully drops you off near
the gardener and his massive farm.

"You got the sand flowing again! Great work! Now we just need to wait until we
have enough sand to filter the water for Snow Island and we'll have snow again
in no time."

While you wait, one of the Elves that works with the gardener heard how good you
are at solving problems and would like your help. He needs to get his steps in
for the day, and so he'd like to know which garden plots he can reach with
exactly his remaining 64 steps.

He gives you an up-to-date map (your puzzle input) of his starting position (S),
garden plots (.), and rocks (#). For example:

...........
.....###.#.
.###.##..#.
..#.#...#..
....#.#....
.##..S####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........

The Elf starts at the starting position (S) which also counts as a garden plot.
Then, he can take one step north, south, east, or west, but only onto tiles that
are garden plots. This would allow him to reach any of the tiles marked O:

...........
.....###.#.
.###.##..#.
..#.#...#..
....#O#....
.##.OS####.
.##..#...#.
.......##..
.##.#.####.
.##..##.##.
...........

Then, he takes a second step. Since at this point he could be at either tile
marked O, his second step would allow him to reach any garden plot that is one
step north, south, east, or west of any tile that he could have reached after
the first step:

...........
.....###.#.
.###.##..#.
..#.#O..#..
....#.#....
.##O.O####.
.##.O#...#.
.......##..
.##.#.####.
.##..##.##.
...........

After two steps, he could be at any of the tiles marked O above, including the
starting position (either by going north-then-south or by going west-then-east).

A single third step leads to even more possibilities:

...........
.....###.#.
.###.##..#.
..#.#.O.#..
...O#O#....
.##.OS####.
.##O.#...#.
....O..##..
.##.#.####.
.##..##.##.
...........

He will continue like this until his steps for the day have been exhausted.
After a total of 6 steps, he could reach any of the garden plots marked O:

...........
.....###.#.
.###.##.O#.
.O#O#O.O#..
O.O.#.#.O..
.##O.O####.
.##.O#O..#.
.O.O.O.##..
.##.#.####.
.##O.##.##.
...........

In this example, if the Elf's goal was to get exactly 6 more steps today, he
could use them to reach any of 16 garden plots.

However, the Elf actually needs to get 64 steps today, and the map he's handed
you is much larger than the example map.

Starting from the garden plot marked S on your map, how many garden plots could
the Elf reach in exactly 64 steps?

--- Part Two ---
The Elf seems confused by your answer until he realizes his mistake: he was
reading from a list of his favorite numbers that are both perfect squares and
perfect cubes, not his step counter.
The actual number of steps he needs to get today is exactly 26501365.

He also points out that the garden plots and rocks are set up so that the map
repeats infinitely in every direction.

So, if you were to look one additional map-width or map-height out from the edge
of the example map above, you would find that it keeps repeating:

.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##...####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................
.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##..S####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................
.................................
.....###.#......###.#......###.#.
.###.##..#..###.##..#..###.##..#.
..#.#...#....#.#...#....#.#...#..
....#.#........#.#........#.#....
.##...####..##...####..##...####.
.##..#...#..##..#...#..##..#...#.
.......##.........##.........##..
.##.#.####..##.#.####..##.#.####.
.##..##.##..##..##.##..##..##.##.
.................................

This is just a tiny three-map-by-three-map slice of the inexplicably-infinite
farm layout; garden plots and rocks repeat as far as you can see. The Elf still
starts on the one middle tile marked S, though - every other repeated S is
replaced with a normal garden plot (.).

Here are the number of reachable garden plots in this new infinite version of
the example map for different numbers of steps:

    In exactly 6 steps, he can still reach 16 garden plots.
    In exactly 10 steps, he can reach any of 50 garden plots.
    In exactly 50 steps, he can reach 1594 garden plots.
    In exactly 100 steps, he can reach 6536 garden plots.
    In exactly 500 steps, he can reach 167004 garden plots.
    In exactly 1000 steps, he can reach 668697 garden plots.
    In exactly 5000 steps, he can reach 16733044 garden plots.

However, the step count the Elf needs is much larger! Starting from the garden
plot marked S on your infinite map, how many garden plots could the Elf reach in
exactly 26501365 steps?
 */
object DataDefs:
  enum Ground:
    case Rock, Plot
  import Ground.*

  extension (c: Char)
    def toGround: Ground = c match
      case '.' => Plot
      case '#' => Rock

  enum Step:
    case N, S, E, W
  import Step.*

  type Row = Int
  type Col = Int
  case class Pos(row: Row, col: Col, ground: Ground)

  case class Garden(land: Seq[Seq[Pos]]):
    lazy val rows = land.size
    lazy val cols = land(0).size

    private def clamp(row: Row, col: Col): Option[Pos] =
      if 0 <= row && row < rows && 0 <= col && col < cols
      then Some(land(row)(col))
      else None

    private def move(pos: Pos)(step: Step): Option[Pos] = (step match
      case N => clamp(pos.row - 1, pos.col)
      case S => clamp(pos.row + 1, pos.col)
      case E => clamp(pos.row, pos.col + 1)
      case W => clamp(pos.row, pos.col - 1)
    )
    .filter(_.ground == Plot)

    def neighborsOf(pos: Pos): Set[Pos] = Set(N, S, E, W).flatMap(move(pos))

object Parsing:
  import DataDefs.*

  private def parseLine(line: String)(row: Row): Seq[Pos] =
    line.zipWithIndex.map((char, col) => Pos(row, col, char.toGround))

  def parseGarden(lines: Seq[String]): Garden =
    Garden(lines.zipWithIndex.map((line, row) => parseLine(line)(row)))

object Solving:
  import DataDefs.*

  @annotation.tailrec
  private def afterNsteps(posts: Set[Pos])(count: Int)(using garden: Garden): Set[Pos] =
    if count == 0 then posts
    else afterNsteps(posts.flatMap(garden.neighborsOf))(count - 1)

  def solve1(lines: Seq[String])(start: Pos)(count: Int): Int =
    given garden: Garden = Parsing.parseGarden(lines)
    afterNsteps(Set(start))(count).size

  def solve2(lines: Seq[String])(start: Pos)(count: Int): Int =
    given garden: Garden = Parsing.parseGarden(lines)
    afterNsteps(Set(start))(count).size

object Test:
  import DataDefs.*, Ground.*
  private lazy val lines = os.read.lines(os.pwd / "2023" / "21" / "21.test.input.txt")
  private lazy val start = Pos(5, 5, Plot) // I replaced S with . in the input file.
  lazy val res1 = Solving.solve1(lines)(start)(6)
  lazy val res2 = Solving.solve2(lines)(start)(5000)
// Test.res1 // part 1: 16
// Test.res2 // part 2: 16733044

object Main:
  import DataDefs.*, Ground.*
  private lazy val lines = os.read.lines(os.pwd / "2023" / "21" / "21.input.txt")
  private lazy val start = Pos(65, 65, Plot) // I replaced S with . in the input file.
  lazy val res1 = Solving.solve1(lines)(start)(64)
  lazy val res2 = Solving.solve1(lines)(start)(26501365)
// Main.res1 // part 1: 3841
// Main.res2 // part 2: 636391426712747
