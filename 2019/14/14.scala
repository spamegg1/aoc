package aoc2019.day14

object DataDefs:
  val ORE  = "ORE"
  val FUEL = "FUEL"
  type Mater  = String
  type Chem   = (mat: Mater, amt: Long)
  type React  = (out: Chem, ins: Seq[Chem])
  type Reacts = Map[Mater, React]
  type Chems  = Map[Mater, Long]

  extension (long: Long)
    def ceil(that: Long) = long / that + (if long % that == 0L then 0L else 1L)

  extension (chems: Chems)
    def modify(mat: Mater, amt: Long): Chems = chems.updated(mat, chems(mat) + amt)
    def make(chem: Chem)(using reacts: Reacts): Chems =
      if chem.mat == ORE || chem.amt <= chems(chem.mat) then
        chems.modify(chem.mat, -chem.amt)
      else
        val extra  = chem.amt - chems(chem.mat)
        val output = reacts(chem.mat).out.amt
        val mult   = extra.ceil(output)
        reacts(chem.mat).ins
          .foldLeft[Chems](chems): (nextChems, nextChem) =>
            nextChems.make(nextChem.mat, nextChem.amt * mult)
          .modify(chem.mat, mult * output - chem.amt)

object Parsing:
  import DataDefs.*

  def parseChem(str: String): Chem = str match
    case s"$amt $name" => (name, amt.toLong)

  def parseIns(ins: String): Seq[Chem] = ins.split(", ").toSeq.map(parseChem)

  def parseLine(line: String): React = line match
    case s"$ins => $out" => (parseChem(out), parseIns(ins))

  def parse(lines: Seq[String]): Reacts = lines
    .map(parseLine)
    .map(react => react.out.mat -> react)
    .toMap

object Solving:
  import DataDefs.*

  def fuel(amt: Long)(using reacts: Reacts) =
    val chems = Map[Mater, Long]().withDefaultValue(0L)
    val chem  = (mat = FUEL, amt = amt)
    val res   = chems.make(chem)
    -res(ORE)

  def binarySearch(start: Long, end: Long)(using reacts: Reacts, threshold: Long): Long =
    if end <= start then start
    else
      val mid  = (start + end) / 2
      val cost = fuel(mid)
      if threshold < cost then binarySearch(start, mid - 1)
      else if cost < threshold then binarySearch(mid + 1, end)
      else mid

  def solve1(lines: Seq[String]) = fuel(1L)(using Parsing.parse(lines))
  def solve2(lines: Seq[String]) =
    given reacts: Reacts  = Parsing.parse(lines)
    given threshold: Long = 1000000000000L
    binarySearch(1L, threshold)

object Test:
  lazy val file1 = os.pwd / "2019" / "14" / "14.test.input.1.txt"
  lazy val file2 = os.pwd / "2019" / "14" / "14.test.input.2.txt"
  lazy val file3 = os.pwd / "2019" / "14" / "14.test.input.3.txt"
  lazy val file4 = os.pwd / "2019" / "14" / "14.test.input.4.txt"
  lazy val file5 = os.pwd / "2019" / "14" / "14.test.input.5.txt"
  lazy val files = Seq(file1, file2, file3, file4, file5)
  lazy val lines = files map os.read.lines
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2

object Main:
  lazy val file  = os.pwd / "2019" / "14" / "14.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 31, 165, 13312, 180697, 2210736
  println(Test.res2) // part 2: 34482758621, 6323777403, 82892753, 5586022, 460664
  println(Main.res1) // part 1: 1590844
  println(Main.res2) // part 2: 1184209 (it was off by 1)
