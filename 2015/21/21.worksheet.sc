object Items:
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
    lazy val cost    = weapon.cost + armor.map(_.cost).sum + rings.map(_.cost).sum
    lazy val defense = weapon.armor + armor.map(_.armor).sum + rings.map(_.armor).sum
    lazy val attack  = weapon.damage + armor.map(_.damage).sum + rings.map(_.damage).sum

  case class Shop(weapons: List[Item], armors: List[Item], rings: List[Item]):
    lazy val allArmors = Nil :: armors.map(List(_))
    lazy val allRings = (Nil :: rings.map(List(_))) ++ rings.combinations(2).map(_.toList)
    lazy val allGears =
      for
        weapon <- weapons
        armor  <- allArmors
        rings  <- allRings
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
    def winner     = if player.hp <= 0 then Winner.Computer else Winner.User

object Parsing:
  import Items.*

  def parseLine(kind: Kind)(line: String): Item =
    val parsed                   = line.split(" +").toSeq
    val name                     = parsed.head
    val Seq(cost, damage, armor) = parsed.tail.map(_.toInt)
    Item(name, kind, cost, damage, armor)

  def parseSection(lines: Seq[String]): List[Item] =
    val (header, list) = (lines.head, lines.tail.toList)
    val kind = header match
      case s"$kind: $stuff" => kind.toKind
    list.map(parseLine(kind))

  def parseShop(lines: String): Shop =
    val separated                    = lines.split("\n\n").map(_.split("\n").toSeq)
    val Array(weapons, armor, rings) = separated.map(parseSection)
    Shop(weapons, armor, rings)

object Solving:
  import Items.*, Characters.*, Agent.*, Game.*

  def playGame(start: State): Winner =
    var state = start
    while !state.isGameOver do state.next
    state.winner

  def tryGear(gear: Gear) =
    val boss   = Character(Boss, 104, 8, 1) // hard-coded!
    val player = Character(gear)
    val start  = State(player, boss)
    playGame(start)

  def solve1(shop: Shop) =
    shop.allGears.filter(tryGear(_) == Winner.User).map(_.cost).min

  def solve2(shop: Shop) =
    shop.allGears.filter(tryGear(_) == Winner.Computer).map(_.cost).max

object Test:
  import Items.*, Characters.*, Agent.*, Game.*
  lazy val player = Character(Player, 8, 5, 5)
  lazy val boss   = Character(Boss, 12, 7, 2)
  lazy val start  = State(player, boss, Turn.User)
  lazy val res    = Solving.playGame(start)
// Test.res // part 1: Winner = User

object Main:
  lazy val file  = os.pwd / "2015" / "21" / "21.input.shop.txt"
  lazy val lines = os.read(file)
  lazy val shop  = Parsing.parseShop(lines)
  lazy val res1  = Solving.solve1(shop)
  lazy val res2  = Solving.solve2(shop)
// Main.res1 // part 1: 78
// Main.res2 // part 2: 148
