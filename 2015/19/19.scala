package aoc2015.day19

import util.matching.Regex, Regex.Match

object DataDefs:
  type Molecule = String
  type Replace  = String
  type Rule     = (Regex, Replace)
  type Rules    = Map[Regex, Seq[Replace]]

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$find => $replace" => (find.r, replace)

  def parse(lines: Seq[String]): Rules = lines.map(parseRule).groupMap(_._1)(_._2).toMap

object Solving:
  import DataDefs.*

  def replaceOneMatch(repls: Seq[Replace])(molecule: Molecule)(mat: Match) =
    repls.map(repl => molecule.patch(mat.start, repl, mat.end - mat.start))

  def replaceAllMatches(molecule: Molecule)(regex: Regex, repls: Seq[Replace]) =
    regex.findAllMatchIn(molecule).flatMap(replaceOneMatch(repls)(molecule))

  def applyAllRulesToOneMolecule(rules: Rules)(molecule: Molecule) =
    rules.flatMap(replaceAllMatches(molecule))

  def applyAllRulesToAllMolecules(molecules: Set[Molecule])(rules: Rules) =
    molecules.flatMap(applyAllRulesToOneMolecule(rules))

  def solve1(lines: Seq[String])(start: Molecule): Int =
    applyAllRulesToOneMolecule(Parsing.parse(lines))(start).toSet.size

  def solve2(lines: Seq[String])(target: Molecule) =
    val rules     = Parsing.parse(lines)
    var steps     = 0
    var molecules = Set("e")
    while !molecules.contains(target) do
      molecules = applyAllRulesToAllMolecules(molecules)(rules)
      steps += 1
    steps

object Test:
  lazy val file  = os.pwd / "2015" / "19" / "19.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)("HOHOHO") // 7
  lazy val res2  = Solving.solve2(lines)("HOHOHO") // 6

object Main:
  lazy val file1 = os.pwd / "2015" / "19" / "19.input.txt"
  lazy val file2 = os.pwd / "2015" / "19" / "19.input.2.txt"
  lazy val lines = os.read.lines(file1)
  lazy val line  = os.read.lines(file2).head
  lazy val res1  = Solving.solve1(lines)(line)
  lazy val res2  = Solving.solve2(lines)(line)

@main
def run =
  println(Test.res1) // part 1: 7
  // println(Test.res2) // part 2: 6
  println(Main.res1) // part 1: 535
  // println(Main.res2) // part 2:
