/*
--- Day 21: RPG Simulator 20XX ---
Little Henry Case got a new video game for Christmas.
It's an RPG, and he's stuck on a boss. He needs to know what equipment to buy at the shop.
He hands you the controller.

In this game, the player (you) and the enemy (the boss) take turns attacking.
The player always goes first. Each attack reduces the opponent's hit points by at least 1.
The first character at or below 0 hit points loses.

Damage dealt by an attacker each turn is equal to the attacker's damage score
minus the defender's armor score. An attacker always does at least 1 damage.
So, if the attacker has a damage score of 8,
and the defender has an armor score of 3,
the defender loses 5 hit points.
If the defender had an armor score of 300,
the defender would still lose 1 hit point.

Your damage score and armor score both start at zero.
They can be increased by buying items in exchange for gold.
You start with no items and have as much gold as you need.
Your total damage or armor is equal to the sum of those stats
from all of your items. You have 100 hit points.

Here is what the item shop is selling:

Weapons:    Cost  Damage  Armor
Dagger        8     4       0
Shortsword   10     5       0
Warhammer    25     6       0
Longsword    40     7       0
Greataxe     74     8       0

Armor:      Cost  Damage  Armor
Leather      13     0       1
Chainmail    31     0       2
Splintmail   53     0       3
Bandedmail   75     0       4
Platemail   102     0       5

Rings:      Cost  Damage  Armor
Damage +1    25     1       0
Damage +2    50     2       0
Damage +3   100     3       0
Defense +1   20     0       1
Defense +2   40     0       2
Defense +3   80     0       3

You must buy exactly one weapon; no dual-wielding.
Armor is optional, but you can't use more than one.
You can buy 0-2 rings (at most one for each hand).
You must use any items you buy.
The shop only has one of each item, so you can't buy, for example, two rings of Damage +3.

For example, suppose you have 8 hit points, 5 damage, and 5 armor,
       and that the boss has 12 hit points, 7 damage, and 2 armor:
  The player deals 5-2 = 3 damage; the boss goes down to 9 hit points.
  The boss deals 7-5 = 2 damage; the player goes down to 6 hit points.
  The player deals 5-2 = 3 damage; the boss goes down to 6 hit points.
  The boss deals 7-5 = 2 damage; the player goes down to 4 hit points.
  The player deals 5-2 = 3 damage; the boss goes down to 3 hit points.
  The boss deals 7-5 = 2 damage; the player goes down to 2 hit points.
  The player deals 5-2 = 3 damage; the boss goes down to 0 hit points.
In this scenario, the player wins! (Barely.)

You have 100 hit points. The boss's actual stats are in your puzzle input.
What is the least amount of gold you can spend and still win the fight?

--- Part Two ---
Turns out the shopkeeper is working with the boss,
and can persuade you to buy whatever items he wants.
The other rules still apply, and he still only has one of each item.
What is the most amount of gold you can spend and still lose the fight?
 */
object Items:
  // items, gear and shop
  enum Kind:
    case Weapon, Armor, Ring
  import Kind.*

  extension (s: String)
    def toKind = s match
      case "Weapons" => Weapon
      case "Armor"   => Armor
      case "Rings"   => Ring

  case class Item(name: String, kind: Kind, cost: Int, damage: Int, armor: Int)

  case class Gear(weapon: Item, armor: List[Item], rings: List[Item]):
    require(armor.size <= 1)
    require(rings.size <= 2)
    require(weapon.kind == Weapon)
    require(armor.forall(_.kind == Armor))
    require(rings.forall(_.kind == Ring))
    lazy val cost = weapon.cost + armor.map(_.cost).sum + rings.map(_.cost).sum
    lazy val defense = weapon.armor + armor.map(_.armor).sum + rings.map(_.armor).sum
    lazy val attack = weapon.damage + armor.map(_.damage).sum + rings.map(_.damage).sum

  case class Shop(weapons: List[Item], armors: List[Item], rings: List[Item]):
    lazy val allArmors = Nil :: armors.map(List(_))
    lazy val allRings = (Nil :: rings.map(List(_))) ++ rings.combinations(2).map(_.toList)
    lazy val allGears =
      for
        weapon <- weapons
        armor <- allArmors
        rings <- allRings
      yield Gear(weapon, armor, rings)

object Characters:
  import Items.*

  enum Agent:
    case Player, Boss

  case class Character(kind: Agent, var hp: Int, attack: Int, defense: Int):
    def takeDamage(damage: Int): Unit = hp -= math.max(1, damage - defense)

  object Character:
    import Agent.Player
    def apply(gear: Gear) = new Character(Player, 100, gear.attack, gear.defense)

object Game:
  import Characters.*

  enum Turn:
    case User, Computer
    def next = this match
      case User     => Computer
      case Computer => User

  enum Winner:
    case User, Computer

  case class State(player: Character, boss: Character, var turn: Turn = Turn.User):
    def next: Unit =
      turn match
        case Turn.User     => boss.takeDamage(player.attack)
        case Turn.Computer => player.takeDamage(boss.attack)
      turn = turn.next

    def isGameOver = player.hp <= 0 || boss.hp <= 0
    def winner = if player.hp <= 0 then Winner.Computer else Winner.User

object Parsing:
  import Items.*

  def parseLine(kind: Kind)(line: String): Item =
    val parsed = line.split(" +").toSeq
    val name = parsed.head
    val Seq(cost, damage, armor) = parsed.tail.map(_.toInt)
    Item(name, kind, cost, damage, armor)

  def parseSection(lines: Seq[String]): List[Item] =
    val (header, list) = (lines.head, lines.tail.toList)
    val kind = header match
      case s"$kind: $stuff" => kind.toKind
    list.map(parseLine(kind))

  def parseShop(lines: String): Shop =
    val separated = lines.split("\n\n").map(_.split("\n").toSeq)
    val Array(weapons, armor, rings) = separated.map(parseSection)
    Shop(weapons, armor, rings)

object Solving:
  import Items.*, Characters.*, Agent.*, Game.*

  def playGame(start: State): Winner =
    var state = start
    while !state.isGameOver do state.next
    state.winner

  def tryGear(gear: Gear) =
    val boss = Character(Boss, 104, 8, 1) // hard-coded!
    val player = Character(gear)
    val start = State(player, boss)
    playGame(start)

  def solve1(shop: Shop) =
    shop.allGears.filter(tryGear(_) == Winner.User).map(_.cost).min

  def solve2(shop: Shop) =
    shop.allGears.filter(tryGear(_) == Winner.Computer).map(_.cost).max

object Testing:
  import Items.*, Characters.*, Agent.*, Game.*
  private lazy val player = Character(Player, 8, 5, 5)
  private lazy val boss = Character(Boss, 12, 7, 2)
  private lazy val start = State(player, boss, Turn.User)
  lazy val result = Solving.playGame(start)
Testing.result // part 1: Winner = User

object Main:
  private lazy val lines = os.read(os.pwd / "21.input.shop.txt")
  private lazy val shop = Parsing.parseShop(lines)
  lazy val result1 = Solving.solve1(shop)
  lazy val result2 = Solving.solve2(shop)
Main.result1 // part 1: 78
Main.result2 // part 2: 148
