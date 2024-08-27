/*
--- Day 19: Medicine for Rudolph ---
Rudolph the Red-Nosed Reindeer is sick!
His nose isn't shining very brightly, and he needs medicine.

Red-Nosed Reindeer biology isn't similar to regular reindeer biology;
Rudolph is going to need custom-made medicine.
Unfortunately, Red-Nosed Reindeer chemistry isn't
similar to regular reindeer chemistry, either.

The North Pole is equipped with a Red-Nosed Reindeer nuclear fusion/fission plant,
capable of constructing any Red-Nosed Reindeer molecule you need.
It works by starting with some input molecule and then doing
a series of replacements, one per step, until it has the right molecule.

However, the machine has to be calibrated before it can be used.
Calibration involves determining the number of molecules that can
be generated in one step from a given starting point.

For example, imagine a simpler machine that supports only the following replacements:

H => HO
H => OH
O => HH

Given the replacements above and starting with HOH,
the following molecules could be generated:
  HOOH (via H => HO on the first H).
  HOHO (via H => HO on the second H).
  OHOH (via H => OH on the first H).
  HOOH (via H => OH on the second H).
  HHHH (via O => HH).

So, in the example above, there are 4 distinct molecules
(not five, because HOOH appears twice) after one replacement from HOH.
Santa's favorite molecule, HOHOHO, can become 7 distinct molecules
(over nine replacements: six from H, and three from O).

The machine replaces without regard for the surrounding characters.
For example, given the string H2O, the transition H => OO would result in OO2O.

Your puzzle input describes all of the possible replacements and,
at the bottom, the medicine molecule for which you need to calibrate
the machine. How many distinct molecules can be created after all
the different ways you can do one replacement on the medicine molecule?

--- Part Two ---
Now that the machine is calibrated, you're ready to begin molecule fabrication.

Molecule fabrication always begins with just a single electron, e,
and applying replacements one at a time, just like the ones during calibration.

For example, suppose you have the following replacements:

e => H
e => O
H => HO
H => OH
O => HH

If you'd like to make HOH, you start with e, and then make the following replacements:
  e => O to get O
  O => HH to get HH
  H => OH (on the second H) to get HOH

So, you could make HOH after 3 steps.
Santa's favorite molecule, HOHOHO, can be made in 6 steps.

How long will it take to make the medicine?
Given the available replacements and the medicine molecule in your puzzle input,
what is the fewest number of steps to go from e to the medicine molecule?
 */
package day19

import util.matching.Regex, Regex.Match

object DataDefs:
  type Molecule = String
  type Replacement = String
  type Rule = (Regex, Replacement)
  type Rules = Map[Regex, Seq[Replacement]]

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$find => $replace" => (find.r, replace)

  def parse(lines: Seq[String]): Rules = lines.map(parseRule).groupMap(_._1)(_._2).toMap

object Solving:
  import DataDefs.*

  def replaceOneMatch(repls: Seq[Replacement])(molecule: Molecule)(mat: Match) =
    repls.map(repl => molecule.patch(mat.start, repl, mat.end - mat.start))

  def replaceAllMatches(molecule: Molecule)(regex: Regex, repls: Seq[Replacement]) =
    regex.findAllMatchIn(molecule).flatMap(replaceOneMatch(repls)(molecule))

  def applyAllRulesToOneMolecule(rules: Rules)(molecule: Molecule) =
    rules.flatMap(replaceAllMatches(molecule))

  def applyAllRulesToAllMolecules(molecules: Set[Molecule])(rules: Rules) =
    molecules.flatMap(applyAllRulesToOneMolecule(rules))

  def solve1(lines: Seq[String])(start: Molecule): Int =
    applyAllRulesToOneMolecule(Parsing.parse(lines))(start).toSet.size

  def solve2(lines: Seq[String])(target: Molecule) =
    val rules = Parsing.parse(lines)
    var steps = 0
    var molecules = Set("e")
    while !molecules.contains(target) do
      molecules = applyAllRulesToAllMolecules(molecules)(rules)
      steps += 1
    steps

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "19" / "19.test.input.txt")
  lazy val result1 = Solving.solve1(lines)("HOHOHO") // 7
  lazy val result2 = Solving.solve2(lines)("HOHOHO") // 6

object Main:
  private lazy val lines = os.read.lines(os.pwd / "2015" / "19" / "19.input.txt")
  private lazy val line = os.read.lines(os.pwd / "2015" / "19" / "19.input.2.txt").head
  lazy val result1 = Solving.solve1(lines)(line) // 535
  lazy val result2 = Solving.solve2(lines)(line) //

@main
def run = println(Main.result2)
