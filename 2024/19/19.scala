/*
--- Day 19: Linen Layout ---
Today, The Historians take you up to the hot springs on Gear Island!
Very suspiciously, absolutely nothing goes wrong as they begin their
careful search of the vast field of helixes.

Could this finally be your chance to visit the onsen next door?
Only one way to find out.

After a brief conversation with the reception staff at the onsen front desk,
you discover that you don't have the right kind of money to pay the admission fee.
However, before you can leave, the staff get your attention.
Apparently, they've heard about how you helped at the hot springs,
and they're willing to make a deal: if you can simply help
them arrange their towels, they'll let you in for free!

Every towel at this onsen is marked with a pattern of colored stripes.
There are only a few patterns, but for any particular pattern,
the staff can get you as many towels with that pattern as you need.
Each stripe can be white (w), blue (u), black (b), red (r), or green (g).
So, a towel with the pattern ggr would have a green stripe, a green stripe,
and then a red stripe, in that order. (You can't reverse a pattern by flipping
a towel upside-down, as that would cause the onsen logo to face the wrong way.)

The Official Onsen Branding Expert has produced a list of designs -
each a long sequence of stripe colors - that they would like to be able to display.
You can use any towels you want, but all of the towels' stripes must exactly
match the desired design. So, to display the design rgrgr, you could use two
rg towels and then an r towel, an rgr towel and then a gr towel, or even a
single massive rgrgr towel (assuming such towel patterns were actually available).

To start, collect together all of the available towel patterns and the list
of desired designs (your puzzle input). For example:

r, wr, b, g, bwu, rb, gb, br

brwrr
bggr
gbbr
rrbgbr
ubwu
bwurrg
brgr
bbrgwb

The first line indicates the available towel patterns; in this example,
the onsen has unlimited towels with a single red stripe (r),
unlimited towels with a white stripe and then a red stripe (wr), and so on.

After the blank line, the remaining lines each describe a design
the onsen would like to be able to display. In this example,
the first design (brwrr) indicates that the onsen would like to
be able to display a black stripe, a red stripe, a white stripe,
and then two red stripes, in that order.

Not all designs will be possible with the available towels.
In the above example, the designs are possible or impossible as follows:
  brwrr can be made with a br towel, then a wr towel, and then finally an r towel.
  bggr can be made with a b towel, two g towels, and then an r towel.
  gbbr can be made with a gb towel and then a br towel.
  rrbgbr can be made with r, rb, g, and br.
  ubwu is impossible.
  bwurrg can be made with bwu, r, r, and g.
  brgr can be made with br, g, and r.
  bbrgwb is impossible.

In this example, 6 of the eight designs are possible with the available towel patterns.

To get into the onsen as soon as possible, consult your list of towel patterns
and desired designs carefully. How many designs are possible?

--- Part Two ---
The staff don't really like some of the towel arrangements you came up with.
To avoid an endless cycle of towel rearrangement,
maybe you should just give them every possible option.

Here are all of the different ways the above example's designs can be made:

brwrr can be made in two different ways: b, r, wr, r or br, wr, r.
bggr can only be made with b, g, g, and r.
gbbr can be made 4 different ways:
  g, b, b, r
  g, b, br
  gb, b, r
  gb, br
rrbgbr can be made 6 different ways:
  r, r, b, g, b, r
  r, r, b, g, br
  r, r, b, gb, r
  r, rb, g, b, r
  r, rb, g, br
  r, rb, gb, r
bwurrg can only be made with bwu, r, r, and g.
brgr can be made in two different ways: b, r, g, r or br, g, r.
ubwu and bbrgwb are still impossible.

Adding up all of the ways the towels in this example could be
arranged into the desired designs yields 16 (2 + 1 + 4 + 6 + 1 + 2).

They'll let you into the onsen as soon as you have the list. What do you get if you add up the number of different ways you could make each design?
 */
package aoc2024.day19

object DataDefs:
  type Towel   = String
  type Towels  = Seq[Towel]
  type Design  = String
  type Designs = Seq[Design]

  extension (d: Design)
    def isPossible(using towels: Towels): Boolean = towels // part 1
      .filter(t => t.size <= d.size && d.startsWith(t))
      .exists(t => t == d || d.drop(t.size).isPossible)

    def countSolutions(using towels: Towels): Long = towels // part 2
      .filter(t => t.size <= d.size && d.endsWith(t))
      .map(t => if d == t then 1L else d.dropRight(t.size).countSolutions)
      .sum

object Parsing:
  import DataDefs.*
  def parse(lines: Seq[String]): (Towels, Designs) =
    (lines.head.split(", ").toSeq, lines.drop(2))

object Solving:
  import DataDefs.*, collection.mutable.{Map => MMap}

  def solve1(lines: Seq[String]) =
    val (towels, designs) = Parsing.parse(lines)
    given Towels          = towels
    designs.count(_.isPossible)

  private val memo = MMap.empty[Design, Long]

  def solutions(design: Design)(using towels: Towels): Long =
    memo.getOrElseUpdate(design, design.countSolutions)

  def solve2(lines: Seq[String]) =
    val (towels, designs) = Parsing.parse(lines)
    given Towels          = towels
    designs.map(solutions).sum

object Test:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "19" / "19.test.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)

object Main:
  lazy val lines   = os.read.lines(os.pwd / "2024" / "19" / "19.input.txt")
  lazy val res1 = Solving.solve1(lines)
  lazy val res2 = Solving.solve2(lines)

@main
def run: Unit =
  // println(Test.res1) // part 1: 6
  // println(Test.res2) // part 2: 16
  // println(Main.res1) // part 1: 360
  // println(Main.res2) // part 2: 577474410989846
  ()
