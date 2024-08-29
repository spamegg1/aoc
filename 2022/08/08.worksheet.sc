/*
--- Day 8: Treetop Tree House ---
The expedition comes across a peculiar patch of tall trees all planted
carefully in a grid. The Elves explain that a previous expedition planted
these trees as a reforestation effort. Now, they're curious if this would
be a good location for a tree house.

First, determine whether there is enough tree cover here to keep a tree house hidden.
To do this, you need to count the number of trees that are visible from outside the
grid when looking directly along a row or column.

The Elves have already launched a quadcopter to generate a map with the height of
each tree (your puzzle input). For example:

30373
25512
65332
33549
35390

Each tree is represented as a single digit whose value is its height,
where 0 is the shortest and 9 is the tallest.

A tree is visible if all of the other trees between it and an edge of
the grid are shorter than it. Only consider trees in the same row or column;
that is, only look up, down, left, or right from any given tree.

All of the trees around the edge of the grid are visible -
since they are already on the edge, there are no trees to block the view.
In this example, that only leaves the interior nine trees to consider:
  The top-left 5 is visible from the left and top.
    (It isn't visible from the right or bottom
    since other trees of height 5 are in the way.)
  The top-middle 5 is visible from the top and right.
  The top-right 1 is not visible from any direction;
    for it to be visible, there would need to only be trees of
    height 0 between it and an edge.
  The left-middle 5 is visible, but only from the right.
  The center 3 is not visible from any direction;
    for it to be visible, there would need to be only trees
    of at most height 2 between it and an edge.
  The right-middle 3 is visible from the right.
  In the bottom row, the middle 5 is visible, but the 3 and 4 are not.

With 16 trees visible on the edge and another 5 visible in the interior,
a total of 21 trees are visible in this arrangement.

Consider your map; how many trees are visible from outside the grid?

--- Part Two ---
Content with the amount of tree cover available, the Elves just need to know the
best spot to build their tree house: they would like to be able to see a lot of trees.

To measure the viewing distance from a given tree, look up, down, left, and right
from that tree; stop if you reach an edge or at the first tree that is the same height
or taller than the tree under consideration. (If a tree is right on the edge, at
least one of its viewing distances will be zero.)

The Elves don't care about distant trees taller than those found by the rules above;
the proposed tree house has large eaves to keep it dry, so they wouldn't be able to
see higher than the tree house anyway.

In the example above, consider the middle 5 in the second row:

30373
25512
65332
33549
35390

  Looking up, its view is not blocked; it can see 1 tree (of height 3).
  Looking left, its view is blocked immediately; it can see only 1 tree
    (of height 5, right next to it).
  Looking right, its view is not blocked; it can see 2 trees.
  Looking down, its view is blocked eventually; it can see 2 trees
    (one of height 3, then the tree of height 5 that blocks its view).

A tree's scenic score is found by multiplying together its viewing distance in each
of the four directions. For this tree, this is 4 (found by multiplying 1 * 1 * 2 * 2).

However, you can do even better:
consider the tree of height 5 in the middle of the fourth row:

30373
25512
65332
33549
35390

  Looking up, its view is blocked at 2 trees (by another tree with a height of 5).
  Looking left, its view is not blocked; it can see 2 trees.
  Looking down, its view is also not blocked; it can see 1 tree.
  Looking right, its view is blocked at 2 trees (by a massive tree of height 9).

This tree's scenic score is 8 (2 * 2 * 1 * 2); this is the ideal spot for the tree house.

Consider each tree on your map. What is the highest scenic score possible for any tree?
 */
object DataDefs:
  type Row = Seq[Int]
  type Col = Seq[Int]

  case class Grid(trees: Seq[Row]):
    private lazy val rows = trees.size
    private lazy val cols = trees.head.size
    private def getCol(col: Int): Col = trees.map(_(col))

    private def leftVisible(row: Int, col: Int) =
      trees(row).take(col).forall(_ < trees(row)(col))
    private def rightVisible(row: Int, col: Int) =
      trees(row).drop(col + 1).forall(_ < trees(row)(col))
    private def topVisible(row: Int, col: Int) =
      getCol(col).take(row).forall(_ < trees(row)(col))
    private def botVisible(row: Int, col: Int) =
      getCol(col).drop(row + 1).forall(_ < trees(row)(col))

    private def isVisible(row: Int, col: Int): Boolean =
      leftVisible(row, col) || rightVisible(row, col) ||
        topVisible(row, col) || botVisible(row, col)

    lazy val countVisible = // part 1
      (for
        row <- 0 until rows
        col <- 0 until cols
        if isVisible(row, col)
      yield 1).sum

    private def look(trees: Row, tree: Int): Int =
      val (visible, invisible) = (trees.takeWhile(_ < tree), trees.dropWhile(_ < tree))
      if invisible.nonEmpty then visible.size + 1 else visible.size

    private def lookUp(row: Int, col: Int): Int =
      look(getCol(col).take(row).reverse, trees(row)(col))
    private def lookDown(row: Int, col: Int): Int =
      look(getCol(col).drop(row + 1), trees(row)(col))
    private def lookLeft(row: Int, col: Int): Int =
      look(trees(row).take(col).reverse, trees(row)(col))
    private def lookRight(row: Int, col: Int): Int =
      look(trees(row).drop(col + 1), trees(row)(col))

    private def scenicScore(row: Int, col: Int): Int =
      lookUp(row, col) * lookDown(row, col) * lookLeft(row, col) * lookRight(row, col)

    lazy val bestScore = // part 2
      (for
        row <- 0 until rows
        col <- 0 until cols
      yield scenicScore(row, col)).max

object Parsing:
  import DataDefs.*
  private def parseLine(line: String): Seq[Int] = line.toSeq.map(_.asDigit)
  def parse(lines: Seq[String]) = Grid(lines map parseLine)

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = Parsing.parse(lines).countVisible
  def solve2(lines: Seq[String]) = Parsing.parse(lines).bestScore

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2022" / "08" / "08.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 21
// Testing.result2 // part 2: 8

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2022" / "08" / "08.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 1845
// Main.result2 // part 2: 230112
