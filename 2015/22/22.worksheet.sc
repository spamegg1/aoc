/*
--- Day 22: Wizard Simulator 20XX ---
Little Henry Case decides that defeating bosses with swords and stuff is boring.
Now he's playing the game with a wizard.
Of course, he gets stuck on another boss and needs your help again.

In this version, combat still proceeds with the player and the boss
taking alternating turns. The player still goes first.
Now, however, you don't get any equipment;
instead, you must choose one of your spells to cast.
The first character at or below 0 hit points loses.

Since you're a wizard, you don't get to wear armor, and you can't attack normally.
However, since you do magic damage, your opponent's armor is ignored,
and so the boss effectively has zero armor as well.
As before, if armor (from a spell, in this case) would reduce damage below 1,
it becomes 1 instead - that is, the boss' attacks always deal at least 1 damage.

On each of your turns, you must select one of your spells to cast.
If you cannot afford to cast any spell, you lose.
Spells cost mana; you start with 500 mana, but have no maximum limit.
You must have enough mana to cast a spell, and its cost is immediately deducted
when you cast it. Your spells are Magic Missile, Drain, Shield, Poison, and Recharge.
  Magic Missile costs 53 mana. It instantly does 4 damage.
  Drain costs 73 mana. It instantly does 2 damage and heals you for 2 hit points.
  Shield costs 113 mana. It starts an effect that lasts for 6 turns.
    While it is active, your armor is increased by 7.
  Poison costs 173 mana. It starts an effect that lasts for 6 turns.
    At the start of each turn while it is active, it deals the boss 3 damage.
  Recharge costs 229 mana. It starts an effect that lasts for 5 turns.
    At the start of each turn while it is active, it gives you 101 new mana.

Effects all work the same way.
Effects apply at the start of both the player's turns and the boss' turns.
Effects are created with a timer (the number of turns they last);
at the start of each turn, after they apply any effect they have,
their timer is decreased by one. If this decreases the timer to zero,
the effect ends. You cannot cast a spell that would start an effect which
is already active. However, effects can be started on the same turn they end.

For example, suppose the player has 10 hit points and 250 mana,
and that the boss has 13 hit points and 8 damage:
-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 13 hit points
Player casts Poison.
-- Boss turn --
- Player has 10 hit points, 0 armor, 77 mana
- Boss has 13 hit points
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 damage.
-- Player turn --
- Player has 2 hit points, 0 armor, 77 mana
- Boss has 10 hit points
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.
-- Boss turn --
- Player has 2 hit points, 0 armor, 24 mana
- Boss has 3 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

Now, suppose the same initial conditions, except that the boss has 14 hit points instead:
-- Player turn --
- Player has 10 hit points, 0 armor, 250 mana
- Boss has 14 hit points
Player casts Recharge.
-- Boss turn --
- Player has 10 hit points, 0 armor, 21 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 4.
Boss attacks for 8 damage!
-- Player turn --
- Player has 2 hit points, 0 armor, 122 mana
- Boss has 14 hit points
Recharge provides 101 mana; its timer is now 3.
Player casts Shield, increasing armor by 7.
-- Boss turn --
- Player has 2 hit points, 7 armor, 110 mana
- Boss has 14 hit points
Shield's timer is now 5.
Recharge provides 101 mana; its timer is now 2.
Boss attacks for 8 - 7 = 1 damage!
-- Player turn --
- Player has 1 hit point, 7 armor, 211 mana
- Boss has 14 hit points
Shield's timer is now 4.
Recharge provides 101 mana; its timer is now 1.
Player casts Drain, dealing 2 damage, and healing 2 hit points.
-- Boss turn --
- Player has 3 hit points, 7 armor, 239 mana
- Boss has 12 hit points
Shield's timer is now 3.
Recharge provides 101 mana; its timer is now 0.
Recharge wears off.
Boss attacks for 8 - 7 = 1 damage!
-- Player turn --
- Player has 2 hit points, 7 armor, 340 mana
- Boss has 12 hit points
Shield's timer is now 2.
Player casts Poison.
-- Boss turn --
- Player has 2 hit points, 7 armor, 167 mana
- Boss has 12 hit points
Shield's timer is now 1.
Poison deals 3 damage; its timer is now 5.
Boss attacks for 8 - 7 = 1 damage!
-- Player turn --
- Player has 1 hit point, 7 armor, 167 mana
- Boss has 9 hit points
Shield's timer is now 0.
Shield wears off, decreasing armor by 7.
Poison deals 3 damage; its timer is now 4.
Player casts Magic Missile, dealing 4 damage.
-- Boss turn --
- Player has 1 hit point, 0 armor, 114 mana
- Boss has 2 hit points
Poison deals 3 damage. This kills the boss, and the player wins.

You start with 50 hit points and 500 mana points.
The boss's actual stats are in your puzzle input.
What is the least amount of mana you can spend and still win the fight?
(Do not include mana recharge effects as "spending" negative mana.)

--- Part Two ---
On the next run through the game, you increase the difficulty to hard.
At the start of each player turn (before any other effects apply),
you lose 1 hit point. If this brings you to or below 0 hit points, you lose.
With the same starting stats for you and the boss,
what is the least amount of mana you can spend and still win the fight?
 */
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
    case Drain extends Spell(73, 2, 2, None)
    case Shield extends Spell(113, 0, 0, Some(Effect(Armor, 6, 7)))
    case Poison extends Spell(173, 0, 0, Some(Effect(Damage, 6, 3)))
    case Recharge extends Spell(229, 0, 0, Some(Effect(ManaRegen, 5, 101)))

    def enoughMana(playerMana: Int): Boolean = mana <= playerMana
    def effectNotActive(kinds: List[Kind]): Boolean = effect match
      case None        => true
      case Some(value) => !kinds.contains(value.kind)
    def dealDamage = damage
    def manaCost = mana
    def healHp = heal
    def getEffect = effect match
      case None        => Nil
      case Some(value) => List(value)

object Characters:
  import Effects.*, Kind.*, Spells.*

  case class Character(hp: Int, mana: Int, attack: Int, defense: Int = 0):
    def takeDamage(damage: Int) = copy(hp = hp - math.max(damage - defense, 1)) // B -> P
    def dealDamage(damage: Int) = copy(hp = hp - damage) // P -> B
    def heal(healHp: Int) = copy(hp = hp + healHp)
    def spendMana(manaCost: Int) = copy(mana = mana - manaCost)
    def hasEnoughManaFor = Spell.values.toList.filter(_.enoughMana(mana))
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
    def kinds = effects.map(_.kind)
    def possibleSpells = player.hasEnoughManaFor.filter(_.effectNotActive(kinds))
    def isGameOver = player.hp <= 0 || boss.hp <= 0 || possibleSpells.isEmpty
    def winner = if boss.hp <= 0 then Player else Boss

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

    def attack: State = copy(player = player.takeDamage(boss.attack), turn = turn.next)
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
  val boss = Character(58, 0, 9, 0)
  val start = State(player, boss, Player, Nil, 0)

  def solve1 =
    val states = collection.mutable.Queue(start)
    var manaCosts = List[(Turn, Int)]()

    while states.nonEmpty do
      val state0 = states.dequeue()
      os.write.append(os.pwd / "2015" / "22" / "22.log.txt", state0.toString())
      state0.turn match
        case Player =>
          val state1 = state0 // part 1
          // val state1 = state0.hardMode // part 2
          if state1.isGameOver then manaCosts ::= (state1.winner, state1.manaSpent)
          else
            val state2 = state1.applyEffects
            if state2.isGameOver then manaCosts ::= (state2.winner, state2.manaSpent)
            else
              val states3 = state2.possibleSpells.map(state2.cast(_))
              val (endStates3, continuedStates3) = states3.partition(_.isGameOver)
              manaCosts :::= endStates3.map(s => (s.winner, s.manaSpent))
              states.enqueueAll(continuedStates3)

        case Boss =>
          val state1 = state0.applyEffects
          if state1.isGameOver then manaCosts ::= (state1.winner, state1.manaSpent)
          else
            val state2 = state1.attack
            if state2.isGameOver then manaCosts ::= (state2.winner, state2.manaSpent)
            else states.enqueue(state2)

    manaCosts.filter(_._1 == Player).minBy(_._2)

object Testing:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  private lazy val player = Character(10, 250, 0, 0)
  private lazy val boss = Character(13, 0, 8, 0)
  lazy val start = State(player, boss, Player, Nil, 0)
  lazy val state1 = start.cast(Poison)
  lazy val state2 = state1.applyEffects
  lazy val state3 = state2.attack
  lazy val state4 = state3.applyEffects
  lazy val state5 = state4.cast(MagicMissile)
  lazy val state6 = state5.applyEffects
// Testing.start // P 10 hp, 0 armor, 250 mana - B 13 hp
// Testing.state1 // P 10 hp, 0 armor, 77 mana - B 13 hp
// Testing.state2 // P 10 hp, 0 armor, 77 mana - B 10 hp
// Testing.state3 // P 2 hp, 0 armor, 77 mana - B 10 hp
// Testing.state4 // P 2 hp, 0 armor, 77 mana - B 7 hp
// Testing.state5 // P 2 hp, 0 armor, 24 mana - B 3 hp
// Testing.state6 // P 2 hp, 0 armor, 24 mana - B 0 hp - winner = Player, 226 mana spent

object Testing2:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  private lazy val player = Character(10, 250, 0, 0)
  private lazy val boss = Character(14, 0, 8, 0)
  lazy val start = State(player, boss, Player, Nil, 0)
  lazy val state1 = start.cast(Recharge) // 229
  lazy val state2 = state1.applyEffects
  lazy val state3 = state2.attack
  lazy val state4 = state3.applyEffects
  lazy val state5 = state4.cast(Shield) // 229 + 113 = 342
  lazy val state6 = state5.applyEffects
  lazy val state7 = state6.attack
  lazy val state8 = state7.applyEffects
  lazy val state9 = state8.cast(Drain) // 342 + 73 = 415
  lazy val state10 = state9.applyEffects
  lazy val state11 = state10.attack
  lazy val state12 = state11.applyEffects
  lazy val state13 = state12.cast(Poison) // 415 + 173 = 588
  lazy val state14 = state13.applyEffects
  lazy val state15 = state14.attack
  lazy val state16 = state15.applyEffects
  lazy val state17 = state16.cast(MagicMissile) // 588 + 53 = 641
  lazy val state18 = state17.applyEffects
// Testing2.start // P 10 hp, 0 armor, 250 mana - B 14 hp
// Testing2.state1 // P 10 hp, 0 armor, 21 mana - B 14 hp - ManaRegen 5
// Testing2.state2 // P 10 hp, 0 armor, 122 mana - B 14 hp - ManaRegen 4
// Testing2.state3 // P 2 hp, 0 armor, 122 mana - B 14 hp - ManaRegen 4
// Testing2.state4 // P 2 hp, 0 armor, 223 mana - B 14 hp - ManaRegen 3
// Testing2.state5 // P 2 hp, 0 armor, 110 mana - B 14 hp - ManaRegen 3, Armor 6
// Testing2.state6 // P 2 hp, 7 armor, 211 mana - B 14 hp - ManaRegen 2, Armor 5
// Testing2.state7 // P 1 hp, 7 armor, 211 mana - B 14 hp - ManaRegen 2, Armor 5
// Testing2.state8 // P 1 hp, 7 armor, 312 mana - B 14 hp - ManaRegen 1, Armor 4
// Testing2.state9 // P 3 hp, 7 armor, 239 mana - B 12 hp - ManaRegen 1, Armor 4
// Testing2.state10 // P 3 hp, 7 armor, 340 mana - B 12 hp - Armor 3
// Testing2.state11 // P 2 hp, 7 armor, 340 mana - B 12 hp - Armor 3
// Testing2.state12 // P 2 hp, 7 armor, 340 mana - B 12 hp - Armor 2
// Testing2.state13 // P 2 hp, 7 armor, 167 mana - B 12 hp - Armor 2, Damage 6
// Testing2.state14 // P 2 hp, 7 armor, 167 mana - B 9 hp - Armor 1, Damage 5
// Testing2.state15 // P 1 hp, 7 armor, 167 mana - B 9 hp - Armor 1, Damage 5
// Testing2.state16 // P 1 hp, 0 armor, 167 mana - B 6 hp - Armor 0, Damage 4
// Testing2.state17 // P 1 hp, 0 armor, 114 mana - B 2 hp - Armor 0, Damage 4
// Testing2.state18 // P 1 hp, 0 armor, 114 mana - B -1 hp - Damage 3 - cost 641

object Main:
  import Characters.*, Game.*, Turn.*, Spells.*, Spell.*
  val player = Character(50, 500, 0, 0)
  val boss = Character(58, 0, 9, 0)
  val start = State(player, boss, Player, Nil, 0)
  lazy val result1 = Solving.solve1
  lazy val result2 = Solving.solve1 // I edited the code instead! see above
// Main.result1 // part 1: 1269
// Main.result2 // part 2: 1309
