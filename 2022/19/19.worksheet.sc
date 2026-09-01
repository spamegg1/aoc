object DataDefs:
  case class Res(ore: Int, clay: Int, obs: Int, geode: Int):
    def +(r: Res)  = Res(ore + r.ore, clay + r.clay, obs + r.obs, geode + r.geode)
    def -(r: Res)  = Res(ore - r.ore, clay - r.clay, obs - r.obs, geode - r.geode)
    def <=(r: Res) = ore <= r.ore && clay <= r.clay && obs <= r.obs && geode <= r.geode

object Parsing:
  import DataDefs.*

  def parseLine(line: String) = line
    .split("\\D+")
    .tail
    .map(_.toInt)
    .toSeq

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def maximize(minutes: Int)(costs: Seq[Int]): Int =
    val Seq(id, ore1, ore2, ore3, clay, ore4, obs) = costs

    val oreCost   = Res(ore1, 0, 0, 0)
    val clayCost  = Res(ore2, 0, 0, 0)
    val obsCost   = Res(ore3, clay, 0, 0)
    val geodeCost = Res(ore4, 0, obs, 0)
    val zero      = Res(0, 0, 0, 0)
    val oreBot    = Res(1, 0, 0, 0)
    val clayBot   = Res(0, 1, 0, 0)
    val obsBot    = Res(0, 0, 1, 0)
    val geodeBot  = Res(0, 0, 0, 1)
    val maxOre    = ore1.max(ore2).max(ore3).max(ore4)
    val maxClay   = clay
    val maxObs    = obs

    def helper(
        time: Int,
        bots: Res,
        res: Res,
        prevCanOre: Boolean,
        prevCanClay: Boolean,
        prevCanObs: Boolean
    ): Int =
      if time == 0 then res.geode
      else if geodeCost <= res then
        helper(time - 1, bots + geodeBot, res + bots - geodeCost, false, false, false)
      else
        val canOre  = oreCost <= res && bots.ore < maxOre
        val canClay = clayCost <= res && bots.clay < maxClay
        val canObs  = obsCost <= res && bots.obs < maxObs

        val first = helper(time - 1, bots, res + bots, canOre, canClay, canObs)
        val second =
          if canOre && !prevCanOre then
            helper(time - 1, bots + oreBot, res + bots - oreCost, false, false, false)
          else 0
        val third =
          if canClay && !prevCanClay then
            helper(time - 1, bots + clayBot, res + bots - clayCost, false, false, false)
          else 0
        val fourth =
          if canObs && !prevCanObs then
            helper(time - 1, bots + obsBot, res + bots - obsCost, false, false, false)
          else 0
        first.max(second).max(third).max(fourth)
      end if
    end helper
    id * helper(minutes, oreBot, zero, false, false, false)
  end maximize

  def solve1(lines: Seq[String])(minutes: Int) = Parsing
    .parse(lines)
    .map(maximize(minutes))
    .sum

  def solve2(lines: Seq[String])(minutes: Int) = Parsing
    .parse(lines.take(3))
    .map(maximize(minutes))
    .product / 6

object Test:
  val file  = os.pwd / "2022" / "19" / "19.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(24)
  val res2  = Solving.solve2(lines)(32)
// Test.res1 // part 1: 33
// Test.res2 // part 2: 1157

object Main:
  val file  = os.pwd / "2022" / "19" / "19.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)(24)
  val res2  = Solving.solve2(lines)(32)
// Main.res1 // part 1: 1487
// Main.res2 // part 2: 13440
