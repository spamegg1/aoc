package aoc2015.day19

import util.matching.Regex, Regex.Match

object DataDefs:
  type Molecule = String
  type Replace  = String
  type Rule     = (Regex, Replace)
  type Rules    = Map[Regex, Seq[Replace]]
  type RevRules = Map[Regex, Replace]

object Parsing:
  import DataDefs.*

  def parseRule(line: String): Rule = line match
    case s"$find => $replace" => find.r -> replace

  def parseRuleRev(line: String): Rule = line match
    case s"$find => $replace" => replace.r -> find

  def parse(lines: Seq[String]): Rules = lines.map(parseRule).groupMap(_._1)(_._2).toMap
  def parseRev(lines: Seq[String]): RevRules = lines.map(parseRuleRev).toMap

object Solving:
  import DataDefs.*

  def replaceOneMatch(repls: Seq[Replace])(molecule: Molecule)(mat: Match) =
    repls.map(repl => molecule.patch(mat.start, repl, mat.end - mat.start))

  def replaceAllMatches(molecule: Molecule)(regex: Regex, repls: Seq[Replace]) =
    regex.findAllMatchIn(molecule).flatMap(replaceOneMatch(repls)(molecule))

  def solve1(lines: Seq[String])(molecule: Molecule): Int = Parsing
    .parse(lines)
    .flatMap(replaceAllMatches(molecule))
    .toSet
    .size

  def solve2(lines: Seq[String])(target: Molecule) =
    val revRules = Parsing.parseRev(lines)
    var steps    = 0
    var molecule = target

    while molecule.distinct != "e" do
      for (regex, replacement) <- revRules do
        regex.findFirstMatchIn(molecule) match
          case None => ()
          case Some(_) =>
            molecule = regex.replaceFirstIn(molecule, replacement)
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
  println(Test.res2) // part 2: 6
  println(Main.res1) // part 1: 535
  println(Main.res2) // part 2: 212
