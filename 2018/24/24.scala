package aoc2018.day24

object DataDefs:
  val groupRegex =
    """(\d+) units each with (\d+) hit points (?:.*)with an attack that does (\d+) (\w+) damage at initiative (\d+)""".r
  val immuneRegex = """immune to (.+?)[;|\)]""".r.unanchored
  val weakRegex   = """weak to (.+?)[;|\)]""".r.unanchored

  enum Army:
    case Draw, Immune, Infection

  case class Attribute(hp: Int, initiative: Int)
  case class Defense(immune: Set[String], weak: Set[String])
  case class Attack(damage: Int, kind: String)
  case class Group(
      army: Army,
      attribute: Attribute,
      defense: Defense,
      attack: Attack,
      var units: Int
  ):
    def effectivePower: Int = units * attack.damage
    def computeDamage(attacker: Group): Int =
      if defense.immune.contains(attacker.attack.kind) then 0
      else if defense.weak.contains(attacker.attack.kind) then 2 * attacker.effectivePower
      else attacker.effectivePower

object Parsing:
  import DataDefs.*, Army.*

  def parseGroup(army: Army, boost: Int, line: String): Group =
    val immune = line match
      case immuneRegex(items) => items.split(", ").toSet
      case _                  => Set()
    val weak = line match
      case weakRegex(items) => items.split(", ").toSet
      case _                => Set()
    line match
      case groupRegex(units, hp, damage, kind, initiative) =>
        val attribute = Attribute(hp.toInt, initiative.toInt)
        val defense   = Defense(immune, weak)
        val attack    = Attack(damage.toInt + boost, kind)
        Group(army, attribute, defense, attack, units.toInt)

  def parse(lines: Seq[String])(boost: Int): Seq[Group] =
    val index     = lines.indexOf("")
    val immune    = lines.slice(1, index).map(parseGroup(Immune, boost, _))
    val infection = lines.drop(index + 2).map(parseGroup(Infection, 0, _))
    immune ++ infection

object Solving:
  import DataDefs.*, Army.*

  @annotation.tailrec
  def fight(groups: Seq[Group]): (Army, Int) =
    val previous = groups.map(_.units).sum
    val targetSelection =
      groups.sortBy(group => (-group.effectivePower, -group.attribute.initiative))

    val targets = targetSelection.foldLeft(Map.empty[Group, Group]): (tgts, next) =>
      val candidate = groups
        .filterNot(tgts.contains)
        .filter(_.army != next.army)
        .filter(_.units > 0)
        .filter(_.computeDamage(next) > 0)
        .maxByOption: target =>
          (target.computeDamage(next), target.effectivePower, target.attribute.initiative)
      candidate match
        case Some(target) => tgts.updated(target, next)
        case None         => tgts

    targets.toSeq
      .sortBy((target, attacker) => -attacker.attribute.initiative)
      .foreach: (target, attacker) =>
        target.units =
          (target.units - target.computeDamage(attacker) / target.attribute.hp).max(0)

    if groups.map(_.units).sum == previous then (Draw, -1)
    else if groups.filter(_.units > 0).map(_.army).toSet.size == 1 then
      (groups.filter(_.units > 0).head.army, groups.map(_.units).sum)
    else fight(groups)
  end fight

  def solve1(lines: Seq[String]) =
    val groups          = Parsing.parse(lines)(0)
    val (winner, units) = fight(groups)
    units

  def solve2(lines: Seq[String]) =
    @annotation.tailrec
    def helper(boost: Int): Int =
      val groups          = Parsing.parse(lines)(boost)
      val (winner, units) = fight(groups)
      if winner == Immune then units else helper(boost + 1)
    helper(1)

object Test:
  lazy val file  = os.pwd / "2018" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2018" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 5216
  println(Test.res2) // part 2: 51
  println(Main.res1) // part 1: 21070
  println(Main.res2) // part 2: 7500
