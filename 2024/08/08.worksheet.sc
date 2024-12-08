/*
--- Day 8: Resonant Collinearity ---
You find yourselves on the roof of a top-secret Easter Bunny installation.

While The Historians do their thing, you take a look at the familiar huge antenna.
Much to your surprise, it seems to have been reconfigured to emit a signal that
makes people 0.1% more likely to buy Easter Bunny brand
Imitation Mediocre Chocolate as a Christmas gift! Unthinkable!

Scanning across the city, you find that there are actually many such antennas.
Each antenna is tuned to a specific frequency indicated by a single lowercase letter,
uppercase letter, or digit. You create a map (your puzzle input) of these antennas.
For example:

............
........0...
.....0......
.......0....
....0.......
......A.....
............
............
........A...
.........A..
............
............

The signal only applies its nefarious effect at specific antinodes based on
the resonant frequencies of the antennas. In particular, an antinode occurs
at any point that is perfectly in line with two antennas of the same frequency
but only when one of the antennas is twice as far away as the other.
This means that for any pair of antennas with the same frequency,
there are two antinodes, one on either side of them.

So, for these two antennas with frequency a,
they create the two antinodes marked with #:

..........
...#......
..........
....a.....
..........
.....a....
..........
......#...
..........
..........

Adding a third antenna with the same frequency creates several more antinodes.
It would ideally add four antinodes, but two are off the right side of the map,
so instead it adds only two:

..........
...#......
# .........
....a.....
........a.
.....a....
..#.......
......#...
..........
..........

Antennas with different frequencies don't create antinodes;
A and a count as different frequencies.
However, antinodes can occur at locations that contain antennas.
In this diagram, the lone antenna with frequency capital A
creates no antinodes but has a lowercase-a-frequency antinode at its location:

..........
...#......
# .........
....a.....
........a.
.....a....
..#.......
......A...
..........
..........

The first example has antennas with two different frequencies,
so the antinodes they create look like this,
plus an antinode overlapping the topmost A-frequency antenna:

......#....#
...#....0...
....#0....#.
..#....0....
....0....#..
.#....A.....
...#........
# ......#....
........A...
.........A..
..........#.
..........#.

Because the topmost A-frequency antenna overlaps with a 0-frequency antinode,
there are 14 total unique locations that contain
an antinode within the bounds of the map.

Calculate the impact of the signal.
How many unique locations within the bounds of the map contain an antinode?

--- Part Two ---
Watching over your shoulder as you work, one of The Historians asks
if you took the effects of resonant harmonics into your calculations.

Whoops!

After updating your model, it turns out that an antinode occurs at any
grid position exactly in line with at least two antennas of the same
frequency, regardless of distance. This means that some of the new
antinodes will occur at the position of each antenna
(unless that antenna is the only one of its frequency).

So, these three T-frequency antennas now create many antinodes:

T....#....
...T......
.T....#...
.........#
..#.......
..........
...#......
..........
....#.....
..........

In fact, the three T-frequency antennas are all exactly in line with two antennas,
so they are all also antinodes! This brings the total number of antinodes
in the above example to 9.

The original example now has 34 antinodes,
including the antinodes that appear on every antenna:

##....#....#
.#.#....0...
..#.#0....#.
..##...0....
....0....#..
.#...#A....#
...#..#.....
#....#.#....
..#.....A...
....#....A..
.#........#.
...#......##

Calculate the impact of the signal using this updated model.
How many unique locations within the bounds of the map contain an antinode?

 */
import language.experimental.namedTuples

object DataDefs:
  type Pos = (row: Int, col: Int)

  extension (p: Pos)
    def dist(that: Pos) = math.abs(p.row - that.row) + math.abs(p.col - that.col)

    def antinodes(that: Pos): Seq[Pos] = // assume p.row <= that.row
      val vert = math.abs(p.row - that.row)
      val horz = math.abs(p.col - that.col)
      if p.col <= that.col then
        Seq((p.row - vert, p.col - horz), (that.row + vert, that.col + horz))
      else Seq((p.row - vert, p.col + horz), (that.row + vert, that.col - horz))

    def inBounds(size: Int) =
      0 <= p.row && p.row < size && 0 <= p.col && p.col < size

  extension (ps: Seq[Pos]) // part 2
    def antinodes: Seq[Pos] = ps

  type Antenna = (pos: Pos, freq: Char)

object Parsing:
  import DataDefs.*

  def parseLine(line: String, row: Int): Seq[Antenna] = line.zipWithIndex
    .filter((freq, _) => freq != '.')
    .map((freq, col) => ((row, col), freq))

  def parse(lines: Seq[String]): Map[Char, Seq[Pos]] = lines.zipWithIndex
    .flatMap(parseLine.tupled)
    .groupMap(_.freq)(_.pos)
    .toMap

object Solving:
  import DataDefs.*

  def pairAntinodes(ps: Seq[Pos]): Seq[Pos] = ps // part 1
    .combinations(2)
    .flatMap(pair => pair.head.antinodes(pair.last))
    .toSeq

  def lineAntinodes(ps: Seq[Pos]): Seq[Pos] = ps // part 2
    .combinations(2)
    .toSeq
    .flatMap(_.antinodes)

  def solve1(lines: Seq[String])(size: Int) = Parsing
    .parse(lines)
    .values
    .flatMap(pairAntinodes)
    .filter(_.inBounds(size))
    .toSet
    .size

  def solve2(lines: Seq[String])(size: Int) = Parsing
    .parse(lines)
    .values
    .flatMap(lineAntinodes)
    .filter(_.inBounds(size))
    .toSet
    .size

object Testing:
  lazy val lines = os.read.lines(os.pwd / "2024" / "08" / "08.test.input.txt")
  lazy val result1 = Solving.solve1(lines)(12)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 14
// Testing.result2 // part 2: 34

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2024" / "08" / "08.input.txt")
  lazy val result1 = Solving.solve1(lines)(50)
  lazy val result2 = Solving.solve2(lines)(50)
// Main.result1 // part 1: 276
// Main.result2 // part 2: 991
