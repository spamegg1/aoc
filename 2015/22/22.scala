package aoc2015.day22

object Effects:
  enum Kind:
    case Armor, Damage, ManaRegen
  import Kind.*

  case class Effect(kind: Kind, timer: Int, value: Int):
    def tick = this match
      case Effect(Armor, 1, _) => Effect(Armor, 0, 0)
      case _                   => copy(timer = timer - 1)

object Spells:
  import Effects.*, Kind.*

  enum Spell(mana: Int, damage: Int, heal: Int, effect: Option[Effect]):
    case MagicMissile extends Spell(53, 4, 0, None)
    case Drain        extends Spell(73, 2, 2, None)
    case Shield       extends Spell(113, 0, 0, Some(Effect(Armor, 6, 7)))
    case Poison       extends Spell(173, 0, 0, Some(Effect(Damage, 6, 3)))
    case Recharge     extends Spell(229, 0, 0, Some(Effect(ManaRegen, 5, 101)))

    def enoughMana(playerMana: Int): Boolean = mana <= playerMana
    def effectNotActive(kinds: List[Kind]): Boolean = effect match
      case None        => true
      case Some(value) => !kinds.contains(value.kind)
    def dealDamage = damage
    def manaCost   = mana
    def healHp     = heal
    def getEffect = effect match
      case None        => Nil
      case Some(value) => List(value)

object Characters:
  import Effects.*, Kind.*, Spells.*

  case class Character(hp: Int, mana: Int, attack: Int, defense: Int = 0):
    def takeDamage(damage: Int)  = copy(hp = hp - math.max(damage - defense, 1)) // B -> P
    def dealDamage(damage: Int)  = copy(hp = hp - damage)                        // P -> B
    def heal(healHp: Int)        = copy(hp = hp + healHp)
    def spendMana(manaCost: Int) = copy(mana = mana - manaCost)
    def hasEnoughManaFor         = Spell.values.toList.filter(_.enoughMana(mana))
    def applyEffect(effect: Effect) = effect.kind match
      case Armor     => copy(defense = effect.value)
      case Damage    => copy(hp = hp - math.max(effect.value - defense, 1))
      case ManaRegen => copy(mana = mana + effect.value)

object Game:
  import Effects.*, Kind.*, Spells.*, Spell.*, Characters.*

  enum Turn:
    case Player, Boss
    def next = this match
      case Player => Boss
      case Boss   => Player
  import Turn.*

  case class State(
      player: Character,
      boss: Character,
      turn: Turn,
      effects: List[Effect],
      manaSpent: Int
  ):
    def kinds          = effects.map(_.kind)
    def possibleSpells = player.hasEnoughManaFor.filter(_.effectNotActive(kinds))
    def isGameOver     = player.hp <= 0 || boss.hp <= 0 || possibleSpells.isEmpty
    def winner         = if boss.hp <= 0 then Player else Boss

    def applyEffects: State =
      val nextPlayer = effects
        .filterNot(_.kind == Damage)
        .foldLeft(player)((p, effect) => p.applyEffect(effect))
      val nextBoss = effects
        .filter(_.kind == Damage)
        .foldLeft(boss)((b, effect) => b.applyEffect(effect))
      val nextEffects = effects.map(_.tick)
      val nextPlayer2 = // armor reaching 0 logic
        if nextEffects.contains(Effect(Armor, 0, 0))
        then nextPlayer.applyEffect(Effect(Armor, 0, 0))
        else nextPlayer
      val nextEffects2 = nextEffects.filter(_.timer > 0)
      State(nextPlayer2, nextBoss, turn, nextEffects2, manaSpent)

    // assume player has enough mana and spell effect is not already present.
    def cast(spell: Spell): State = State(
      player.spendMana(spell.manaCost).heal(spell.healHp),
      boss.dealDamage(spell.dealDamage),
      turn.next,
      spell.getEffect ::: effects,
      manaSpent + spell.manaCost
    )

    def attack: State   = copy(player = player.takeDamage(boss.attack), turn = turn.next)
    def hardMode: State = copy(player = player.heal(-1))

    // first apply all effects.
    // then, check if game is over (boss might die from poison!)
    // then, if boss turn, apply boss attack.
    //       else, check available spells (after effects have been applied).
    //             and pick one spell and use it.
    // repeat.

object Solving:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*

  val player = Character(50, 500, 0, 0)
  val boss   = Character(58, 0, 9, 0)
  val start  = State(player, boss, Player, Nil, 0)

  def solve(hard: Boolean) =
    val states    = collection.mutable.Queue(start)
    var manaCosts = List[(Turn, Int)]()

    while states.nonEmpty do
      val state0 = states.dequeue()
      // os.write.append(os.pwd / "2015" / "22" / "22.log.txt", state0.toString())
      state0.turn match
        case Player =>
          val state1 = if hard then state0.hardMode else state0
          if state1.isGameOver then manaCosts ::= (state1.winner, state1.manaSpent)
          else
            val state2 = state1.applyEffects
            if state2.isGameOver then manaCosts ::= (state2.winner, state2.manaSpent)
            else
              val states3 = state2.possibleSpells.map(state2.cast(_))
              val (endStates3, continuedStates3) = states3.partition(_.isGameOver)
              manaCosts :::= endStates3.map(s => (s.winner, s.manaSpent))
              states.enqueueAll(continuedStates3.filter(_.manaSpent < 1500))

        case Boss =>
          val state1 = state0.applyEffects
          if state1.isGameOver then manaCosts ::= (state1.winner, state1.manaSpent)
          else
            val state2 = state1.attack
            if state2.isGameOver then manaCosts ::= (state2.winner, state2.manaSpent)
            else states.enqueue(state2)

    manaCosts.filter(_._1 == Player).minBy(_._2)

object Test:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  lazy val player = Character(10, 250, 0, 0)
  lazy val boss   = Character(13, 0, 8, 0)
  lazy val start  = State(player, boss, Player, Nil, 0)
  lazy val state1 = start.cast(Poison)
  lazy val state2 = state1.applyEffects
  lazy val state3 = state2.attack
  lazy val state4 = state3.applyEffects
  lazy val state5 = state4.cast(MagicMissile)
  lazy val state6 = state5.applyEffects
// Test.start // P 10 hp, 0 armor, 250 mana - B 13 hp
// Test.state1 // P 10 hp, 0 armor, 77 mana - B 13 hp
// Test.state2 // P 10 hp, 0 armor, 77 mana - B 10 hp
// Test.state3 // P 2 hp, 0 armor, 77 mana - B 10 hp
// Test.state4 // P 2 hp, 0 armor, 77 mana - B 7 hp
// Test.state5 // P 2 hp, 0 armor, 24 mana - B 3 hp
// Test.state6 // P 2 hp, 0 armor, 24 mana - B 0 hp - winner = Player, 226 mana spent

object Test2:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  lazy val player  = Character(10, 250, 0, 0)
  lazy val boss    = Character(14, 0, 8, 0)
  lazy val start   = State(player, boss, Player, Nil, 0)
  lazy val state1  = start.cast(Recharge)       // 229
  lazy val state2  = state1.applyEffects
  lazy val state3  = state2.attack
  lazy val state4  = state3.applyEffects
  lazy val state5  = state4.cast(Shield)        // 229 + 113 = 342
  lazy val state6  = state5.applyEffects
  lazy val state7  = state6.attack
  lazy val state8  = state7.applyEffects
  lazy val state9  = state8.cast(Drain)         // 342 + 73 = 415
  lazy val state10 = state9.applyEffects
  lazy val state11 = state10.attack
  lazy val state12 = state11.applyEffects
  lazy val state13 = state12.cast(Poison)       // 415 + 173 = 588
  lazy val state14 = state13.applyEffects
  lazy val state15 = state14.attack
  lazy val state16 = state15.applyEffects
  lazy val state17 = state16.cast(MagicMissile) // 588 + 53 = 641
  lazy val state18 = state17.applyEffects
// Test2.start // P 10 hp, 0 armor, 250 mana - B 14 hp
// Test2.state1 // P 10 hp, 0 armor, 21 mana - B 14 hp - ManaRegen 5
// Test2.state2 // P 10 hp, 0 armor, 122 mana - B 14 hp - ManaRegen 4
// Test2.state3 // P 2 hp, 0 armor, 122 mana - B 14 hp - ManaRegen 4
// Test2.state4 // P 2 hp, 0 armor, 223 mana - B 14 hp - ManaRegen 3
// Test2.state5 // P 2 hp, 0 armor, 110 mana - B 14 hp - ManaRegen 3, Armor 6
// Test2.state6 // P 2 hp, 7 armor, 211 mana - B 14 hp - ManaRegen 2, Armor 5
// Test2.state7 // P 1 hp, 7 armor, 211 mana - B 14 hp - ManaRegen 2, Armor 5
// Test2.state8 // P 1 hp, 7 armor, 312 mana - B 14 hp - ManaRegen 1, Armor 4
// Test2.state9 // P 3 hp, 7 armor, 239 mana - B 12 hp - ManaRegen 1, Armor 4
// Test2.state10 // P 3 hp, 7 armor, 340 mana - B 12 hp - Armor 3
// Test2.state11 // P 2 hp, 7 armor, 340 mana - B 12 hp - Armor 3
// Test2.state12 // P 2 hp, 7 armor, 340 mana - B 12 hp - Armor 2
// Test2.state13 // P 2 hp, 7 armor, 167 mana - B 12 hp - Armor 2, Damage 6
// Test2.state14 // P 2 hp, 7 armor, 167 mana - B 9 hp - Armor 1, Damage 5
// Test2.state15 // P 1 hp, 7 armor, 167 mana - B 9 hp - Armor 1, Damage 5
// Test2.state16 // P 1 hp, 0 armor, 167 mana - B 6 hp - Armor 0, Damage 4
// Test2.state17 // P 1 hp, 0 armor, 114 mana - B 2 hp - Armor 0, Damage 4
// Test2.state18 // P 1 hp, 0 armor, 114 mana - B -1 hp - Damage 3 - cost 641

object Main:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  lazy val player = Character(50, 500, 0, 0)
  lazy val boss   = Character(58, 0, 9, 0)
  lazy val start  = State(player, boss, Player, Nil, 0)
  lazy val res1   = Solving.solve(hard = false)
  lazy val res2   = Solving.solve(hard = true)

@main
def run: Unit =
  println(Main.res1) // part 1: 1269
  println(Main.res2) // part 2: 1309
