/*
--- Day 11: Monkey in the Middle ---
As you finally start making your way upriver, you realize your pack is much lighter than
you remember. Just then, one of the items from your pack goes flying overhead.
Monkeys are playing Keep Away with your missing things!

To get your stuff back, you need to be able to predict where the
monkeys will throw your items. After some careful observation,
you realize the monkeys operate based on how worried you are about each item.

You take some notes (your puzzle input) on the items each monkey
currently has, how worried you are about those items, and how the
monkey makes decisions based on your worry level. For example:

Monkey 0:
  Starting items: 79, 98
  Operation: new = old * 19
  Test: divisible by 23
    If true: throw to monkey 2
    If false: throw to monkey 3

Monkey 1:
  Starting items: 54, 65, 75, 74
  Operation: new = old + 6
  Test: divisible by 19
    If true: throw to monkey 2
    If false: throw to monkey 0

Monkey 2:
  Starting items: 79, 60, 97
  Operation: new = old * old
  Test: divisible by 13
    If true: throw to monkey 1
    If false: throw to monkey 3

Monkey 3:
  Starting items: 74
  Operation: new = old + 3
  Test: divisible by 17
    If true: throw to monkey 0
    If false: throw to monkey 1

Each monkey has several attributes:
  Starting items lists your worry level for each item the monkey is currently
    holding in the order they will be inspected.
  Operation shows how your worry level changes as that monkey inspects an item.
    (An operation like new = old * 5 means that your worry level after the monkey
    inspected the item is five times whatever your worry level was before inspection.)
  Test shows how the monkey uses your worry level to decide where to throw an item next.
    If true shows what happens with an item if the Test was true.
    If false shows what happens with an item if the Test was false.

After each monkey inspects an item but before it tests your worry level,
your relief that the monkey's inspection didn't damage the item causes your
worry level to be divided by three and Stateed down to the nearest Longeger.

The monkeys take turns inspecting and throwing items. On a single monkey's turn,
it inspects and throws all of the items it is holding one at a time and in the
order listed. Monkey 0 goes first, then monkey 1, and so on until each monkey
has had one turn. The process of each monkey taking a single turn is called a State.

When a monkey throws an item to another monkey, the item goes on the end of the
recipient monkey's list. A monkey that starts a State with no items could end up
inspecting and throwing many items by the time its turn comes aState.
If a monkey is holding no items at the start of its turn, its turn ends.

In the above example, the first State proceeds as follows:

Monkey 0:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by 19 to 1501.
    Monkey gets bored with item. Worry level is divided by 3 to 500.
    Current worry level is not divisible by 23.
    Item with worry level 500 is thrown to monkey 3.
  Monkey inspects an item with a worry level of 98.
    Worry level is multiplied by 19 to 1862.
    Monkey gets bored with item. Worry level is divided by 3 to 620.
    Current worry level is not divisible by 23.
    Item with worry level 620 is thrown to monkey 3.
Monkey 1:
  Monkey inspects an item with a worry level of 54.
    Worry level increases by 6 to 60.
    Monkey gets bored with item. Worry level is divided by 3 to 20.
    Current worry level is not divisible by 19.
    Item with worry level 20 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 65.
    Worry level increases by 6 to 71.
    Monkey gets bored with item. Worry level is divided by 3 to 23.
    Current worry level is not divisible by 19.
    Item with worry level 23 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 75.
    Worry level increases by 6 to 81.
    Monkey gets bored with item. Worry level is divided by 3 to 27.
    Current worry level is not divisible by 19.
    Item with worry level 27 is thrown to monkey 0.
  Monkey inspects an item with a worry level of 74.
    Worry level increases by 6 to 80.
    Monkey gets bored with item. Worry level is divided by 3 to 26.
    Current worry level is not divisible by 19.
    Item with worry level 26 is thrown to monkey 0.
Monkey 2:
  Monkey inspects an item with a worry level of 79.
    Worry level is multiplied by itself to 6241.
    Monkey gets bored with item. Worry level is divided by 3 to 2080.
    Current worry level is divisible by 13.
    Item with worry level 2080 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 60.
    Worry level is multiplied by itself to 3600.
    Monkey gets bored with item. Worry level is divided by 3 to 1200.
    Current worry level is not divisible by 13.
    Item with worry level 1200 is thrown to monkey 3.
  Monkey inspects an item with a worry level of 97.
    Worry level is multiplied by itself to 9409.
    Monkey gets bored with item. Worry level is divided by 3 to 3136.
    Current worry level is not divisible by 13.
    Item with worry level 3136 is thrown to monkey 3.
Monkey 3:
  Monkey inspects an item with a worry level of 74.
    Worry level increases by 3 to 77.
    Monkey gets bored with item. Worry level is divided by 3 to 25.
    Current worry level is not divisible by 17.
    Item with worry level 25 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 500.
    Worry level increases by 3 to 503.
    Monkey gets bored with item. Worry level is divided by 3 to 167.
    Current worry level is not divisible by 17.
    Item with worry level 167 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 620.
    Worry level increases by 3 to 623.
    Monkey gets bored with item. Worry level is divided by 3 to 207.
    Current worry level is not divisible by 17.
    Item with worry level 207 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 1200.
    Worry level increases by 3 to 1203.
    Monkey gets bored with item. Worry level is divided by 3 to 401.
    Current worry level is not divisible by 17.
    Item with worry level 401 is thrown to monkey 1.
  Monkey inspects an item with a worry level of 3136.
    Worry level increases by 3 to 3139.
    Monkey gets bored with item. Worry level is divided by 3 to 1046.
    Current worry level is not divisible by 17.
    Item with worry level 1046 is thrown to monkey 1.

After State 1, the monkeys are holding items with these worry levels:

Monkey 0: 20, 23, 27, 26
Monkey 1: 2080, 25, 167, 207, 401, 1046
Monkey 2:
Monkey 3:

Monkeys 2 and 3 aren't holding any items at the end of the State;
they both inspected items during the State and threw them all before the State ended.

This process continues for a few more States:

After State 2, the monkeys are holding items with these worry levels:
Monkey 0: 695, 10, 71, 135, 350
Monkey 1: 43, 49, 58, 55, 362
Monkey 2:
Monkey 3:

After State 3, the monkeys are holding items with these worry levels:
Monkey 0: 16, 18, 21, 20, 122
Monkey 1: 1468, 22, 150, 286, 739
Monkey 2:
Monkey 3:

After State 4, the monkeys are holding items with these worry levels:
Monkey 0: 491, 9, 52, 97, 248, 34
Monkey 1: 39, 45, 43, 258
Monkey 2:
Monkey 3:

After State 5, the monkeys are holding items with these worry levels:
Monkey 0: 15, 17, 16, 88, 1037
Monkey 1: 20, 110, 205, 524, 72
Monkey 2:
Monkey 3:

After State 6, the monkeys are holding items with these worry levels:
Monkey 0: 8, 70, 176, 26, 34
Monkey 1: 481, 32, 36, 186, 2190
Monkey 2:
Monkey 3:

After State 7, the monkeys are holding items with these worry levels:
Monkey 0: 162, 12, 14, 64, 732, 17
Monkey 1: 148, 372, 55, 72
Monkey 2:
Monkey 3:

After State 8, the monkeys are holding items with these worry levels:
Monkey 0: 51, 126, 20, 26, 136
Monkey 1: 343, 26, 30, 1546, 36
Monkey 2:
Monkey 3:

After State 9, the monkeys are holding items with these worry levels:
Monkey 0: 116, 10, 12, 517, 14
Monkey 1: 108, 267, 43, 55, 288
Monkey 2:
Monkey 3:

After State 10, the monkeys are holding items with these worry levels:
Monkey 0: 91, 16, 20, 98
Monkey 1: 481, 245, 22, 26, 1092, 30
Monkey 2:
Monkey 3:

...

After State 15, the monkeys are holding items with these worry levels:
Monkey 0: 83, 44, 8, 184, 9, 20, 26, 102
Monkey 1: 110, 36
Monkey 2:
Monkey 3:

...

After State 20, the monkeys are holding items with these worry levels:
Monkey 0: 10, 12, 14, 26, 34
Monkey 1: 245, 93, 53, 199, 115
Monkey 2:
Monkey 3:

Chasing all of the monkeys at once is impossible; you're going to have to focus
on the two most active monkeys if you want any hope of getting your stuff back.
Count the total number of times each monkey inspects items over 20 States:

Monkey 0 inspected items 101 times.
Monkey 1 inspected items 95 times.
Monkey 2 inspected items 7 times.
Monkey 3 inspected items 105 times.

In this example, the two most active monkeys inspected items 101 and 105 times.
The level of monkey business in this situation can be found by multiplying: 10605.

Figure out which monkeys to chase by counting how many items they inspect over 20 States.
What is the level of monkey business after 20 States of stuff-slinging simian shenanigans?

--- Part Two ---
You're worried you might not ever get your items back.
So worried, in fact, that your relief that a monkey's inspection didn't
damage an item no longer causes your worry level to be divided by three.

Unfortunately, that relief was all that was keeping your worry levels from reaching
ridiculous levels. You'll need to find another way to keep your worry levels manageable.

At this rate, you might be putting up with these
monkeys for a very long time - possibly 10000 rounds!

With these new rules, you can still figure out the monkey business after 10000 rounds.
Using the same example above:

== After round 1 ==
Monkey 0 inspected items 2 times.
Monkey 1 inspected items 4 times.
Monkey 2 inspected items 3 times.
Monkey 3 inspected items 6 times.

== After round 20 ==
Monkey 0 inspected items 99 times.
Monkey 1 inspected items 97 times.
Monkey 2 inspected items 8 times.
Monkey 3 inspected items 103 times.

== After round 1000 ==
Monkey 0 inspected items 5204 times.
Monkey 1 inspected items 4792 times.
Monkey 2 inspected items 199 times.
Monkey 3 inspected items 5192 times.

== After round 2000 ==
Monkey 0 inspected items 10419 times.
Monkey 1 inspected items 9577 times.
Monkey 2 inspected items 392 times.
Monkey 3 inspected items 10391 times.

== After round 3000 ==
Monkey 0 inspected items 15638 times.
Monkey 1 inspected items 14358 times.
Monkey 2 inspected items 587 times.
Monkey 3 inspected items 15593 times.

== After round 4000 ==
Monkey 0 inspected items 20858 times.
Monkey 1 inspected items 19138 times.
Monkey 2 inspected items 780 times.
Monkey 3 inspected items 20797 times.

== After round 5000 ==
Monkey 0 inspected items 26075 times.
Monkey 1 inspected items 23921 times.
Monkey 2 inspected items 974 times.
Monkey 3 inspected items 26000 times.

== After round 6000 ==
Monkey 0 inspected items 31294 times.
Monkey 1 inspected items 28702 times.
Monkey 2 inspected items 1165 times.
Monkey 3 inspected items 31204 times.

== After round 7000 ==
Monkey 0 inspected items 36508 times.
Monkey 1 inspected items 33488 times.
Monkey 2 inspected items 1360 times.
Monkey 3 inspected items 36400 times.

== After round 8000 ==
Monkey 0 inspected items 41728 times.
Monkey 1 inspected items 38268 times.
Monkey 2 inspected items 1553 times.
Monkey 3 inspected items 41606 times.

== After round 9000 ==
Monkey 0 inspected items 46945 times.
Monkey 1 inspected items 43051 times.
Monkey 2 inspected items 1746 times.
Monkey 3 inspected items 46807 times.

== After round 10000 ==
Monkey 0 inspected items 52166 times.
Monkey 1 inspected items 47830 times.
Monkey 2 inspected items 1938 times.
Monkey 3 inspected items 52013 times.

After 10000 rounds, the two most active monkeys inspected items 52166 and 52013 times.
Multiplying these together, the level of monkey business in this situation is now
2713310158.

Worry levels are no longer divided by three after each item is inspected;
you'll need to find another way to keep your worry levels manageable.
Starting again from the initial state in your puzzle input,
what is the level of monkey business after 10000 rounds?
 */
object DataDefs:
  type Id = Long
  type Item = Long
  type Items = List[Item]

  type Op = Long => Long
  type Test = Long => Long

  case class Throw(id: Id, item: Item)
  type Throws = List[Throw]

  case class Monkey(id: Id, op: Op, test: Test, count: Long = 0L):
    def incByOne = copy(count = count + 1L)

  case class MonkeyItems(monkey: Monkey, items: Items, throws: Throws = Nil):
    def receiveThrownItems(newItems: Throws): MonkeyItems =
      MonkeyItems(monkey, newItems.map(_.item) ::: items, throws)

    lazy val removeThrownItems: MonkeyItems = copy(throws = Nil)

    def inspect(using d: Long): MonkeyItems = items match
      case item :: next =>
        val worry = monkey.op(item) / d
        val newThrow = Throw(monkey.test(worry), worry)
        MonkeyItems(monkey.incByOne, next, newThrow :: throws).inspect
      case Nil => this

  case class State(monkeysItems: List[MonkeyItems], throws: Throws = Nil)(using d: Long):
    def oneMonkey(id: Id): State =
      val monkeyItems = monkeysItems.find(_.monkey.id == id).get
      val relevantThrows = throws.filter(_.id == id)
      val irrelevantThrows = throws.filterNot(_.id == id)
      val addedItems = monkeyItems.receiveThrownItems(relevantThrows)
      val inspected = addedItems.inspect // d is used here
      val newThrows = inspected.throws
      val updatedMonkey = inspected.removeThrownItems
      val newMonkeysItems = monkeysItems.map: mi =>
        if mi.monkey.id == id then updatedMonkey else mi
      State(newMonkeysItems, newThrows ::: irrelevantThrows)

    def oneRound: State = monkeysItems
      .sortBy(_.monkey.id)
      .map(_.monkey.id)
      .foldLeft(this)((state, id) => state.oneMonkey(id))

    def manyRounds(rounds: Int) =
      (0 until rounds).foldLeft(this)((state, _) => state.oneRound)

    lazy val monkeyBusiness = monkeysItems
      .map(_.monkey.count)
    // .sorted
    // .reverse
    // .take(2)
    // .product

object Parsing:
  import DataDefs.*

  def parseId(line: String) = line match
    case s"Monkey $id:" => id.toLong

  def parseItems(line: String) = line match
    case s"  Starting items: $items" => items.split(", ").map(_.toLong).toList

  def parseOp(line: String): Op = line match
    case s"  Operation: new = old + old"  => x => x + x
    case s"  Operation: new = old * old"  => x => x * x
    case s"  Operation: new = old + $num" => _ + num.toLong
    case s"  Operation: new = old * $num" => _ * num.toLong

  def parseTest(lines: List[String]): Test =
    val divisor = lines(0) match
      case s"  Test: divisible by $div" => div.toLong
    val ifTrue = lines(1) match
      case s"    If true: throw to monkey $id" => id.toLong
    val ifFalse = lines(2) match
      case s"    If false: throw to monkey $id" => id.toLong
    x => if x % divisor == 0 then ifTrue else ifFalse

  def parseMonkey(monkey: String): MonkeyItems =
    val lines = monkey.split("\n").toList
    val id = parseId(lines.head)
    val items = parseItems(lines(1))
    val op = parseOp(lines(2))
    val test = parseTest(lines.drop(3))
    MonkeyItems(Monkey(id, op, test), items)

  def parseMonkeys(lines: String) = lines.split("\n\n").toList.map(parseMonkey)
  def parse(lines: String)(using d: Long): State = State(parseMonkeys(lines))

object Solving:
  import DataDefs.*

  def solve(lines: String)(rounds: Int)(using d: Long) = Parsing
    .parse(lines)
    .manyRounds(rounds)
    .monkeyBusiness

  def solve1(lines: String) = solve(lines)(20)(using 3L)
  def solve2(lines: String) = solve(lines)(1000)(using 1L)

object Testing:
  private lazy val lines = os.read(os.pwd / "2022" / "11" / "11.test.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Testing.result1 // part 1: 10605
// Testing.result2 // part 2: 2713310158

object Main:
  private lazy val lines = os.read(os.pwd / "2022" / "11" / "11.input.txt")
  lazy val result1 = Solving.solve1(lines)
  lazy val result2 = Solving.solve2(lines)
// Main.result1 // part 1: 58786
// Main.result2 // part 2:
// == After round 1000 ==
// Monkey 0 inspected items 5204 times.
// Monkey 1 inspected items 4792 times.
// Monkey 2 inspected items 199 times.
// Monkey 3 inspected items 5192 times.

// == After round 2000 ==
// Monkey 0 inspected items 10419 times.
// Monkey 1 inspected items 9577 times.
// Monkey 2 inspected items 392 times.
// Monkey 3 inspected items 10391 times.

// == After round 3000 ==
// Monkey 0 inspected items 15638 times.
// Monkey 1 inspected items 14358 times.
// Monkey 2 inspected items 587 times.
// Monkey 3 inspected items 15593 times.

// == After round 4000 ==
// Monkey 0 inspected items 20858 times.
// Monkey 1 inspected items 19138 times.
// Monkey 2 inspected items 780 times.
// Monkey 3 inspected items 20797 times.

// == After round 5000 ==
// Monkey 0 inspected items 26075 times.
// Monkey 1 inspected items 23921 times.
// Monkey 2 inspected items 974 times.
// Monkey 3 inspected items 26000 times.

// == After round 6000 ==
// Monkey 0 inspected items 31294 times.
// Monkey 1 inspected items 28702 times.
// Monkey 2 inspected items 1165 times.
// Monkey 3 inspected items 31204 times.

// == After round 7000 ==
// Monkey 0 inspected items 36508 times.
// Monkey 1 inspected items 33488 times.
// Monkey 2 inspected items 1360 times.
// Monkey 3 inspected items 36400 times.

// == After round 8000 ==
// Monkey 0 inspected items 41728 times.
// Monkey 1 inspected items 38268 times.
// Monkey 2 inspected items 1553 times.
// Monkey 3 inspected items 41606 times.

// == After round 9000 ==
// Monkey 0 inspected items 46945 times.
// Monkey 1 inspected items 43051 times.
// Monkey 2 inspected items 1746 times.
// Monkey 3 inspected items 46807 times.

// == After round 10000 ==
// Monkey 0 inspected items 52166 times.
// Monkey 1 inspected items 47830 times.
// Monkey 2 inspected items 1938 times.
// Monkey 3 inspected items 52013 times.
