/*
--- Day 5: Supply Stacks ---
The expedition can depart as soon as the final supplies have been unloaded from the ships.
Supplies are stored in stacks of marked crates, but because the needed supplies are buried
under many other crates, the crates need to be rearranged.

The ship has a giant cargo crane capable of moving crates between stacks.
To ensure none of the crates get crushed or fall over, the crane operator will
rearrange them in a series of carefully-planned steps.
After the crates are rearranged, the desired crates will be at the top of each stack.

The Elves don't want to interrupt the crane operator during this delicate procedure,
but they forgot to ask her which crate will end up where, and they want to be ready to
unload them as soon as possible so they can embark.

They do, however, have a drawing of the starting stacks of crates and the rearrangement
procedure (your puzzle input). For example:

    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

move 1 from 2 to 1
move 3 from 1 to 3
move 2 from 2 to 1
move 1 from 1 to 2

In this example, there are three stacks of crates.
Stack 1 contains two crates: crate Z is on the bottom, and crate N is on top.
Stack 2 contains three crates; from bottom to top, they are crates M, C, and D.
Finally, stack 3 contains a single crate, P.

Then, the rearrangement procedure is given. In each step of the procedure,
a quantity of crates is moved from one stack to a different stack.
In the first step of the above rearrangement procedure, one crate is moved
from stack 2 to stack 1, resulting in this configuration:

[D]
[N] [C]
[Z] [M] [P]
 1   2   3

In the second step, three crates are moved from stack 1 to stack 3.
Crates are moved one at a time, so the first crate to be moved (D)
ends up below the second and third crates:

        [Z]
        [N]
    [C] [D]
    [M] [P]
 1   2   3

Then, both crates are moved from stack 2 to stack 1.
Again, because crates are moved one at a time, crate C ends up below crate M:

        [Z]
        [N]
[M]     [D]
[C]     [P]
 1   2   3

Finally, one crate is moved from stack 1 to stack 2:

        [Z]
        [N]
        [D]
[C] [M] [P]
 1   2   3

The Elves just need to know which crate will end up on top of each stack;
in this example, the top crates are C in stack 1, M in stack 2, and Z in stack 3,
so you should combine these together and give the Elves the message CMZ.

After the rearrangement procedure completes, what crate ends up on top of each stack?

--- Part Two ---
As you watch the crane operator expertly rearrange the crates,
you notice the process isn't following your prediction.

Some mud was covering the writing on the side of the crane, and you quickly wipe it away.
The crane isn't a CrateMover 9000 - it's a CrateMover 9001.

The CrateMover 9001 is notable for many new and exciting features:
air conditioning, leather seats, an extra cup holder,
and the ability to pick up and move multiple crates at once.

Again considering the example above, the crates begin in the same configuration:

    [D]
[N] [C]
[Z] [M] [P]
 1   2   3

Moving a single crate from stack 2 to stack 1 behaves the same as before:

[D]
[N] [C]
[Z] [M] [P]
 1   2   3

However, the action of moving three crates from stack 1 to stack 3 means that those
three moved crates stay in the same order, resulting in this new configuration:

        [D]
        [N]
    [C] [Z]
    [M] [P]
 1   2   3

Next, as both crates are moved from stack 2 to stack 1, they retain their order as well:

        [D]
        [N]
[C]     [Z]
[M]     [P]
 1   2   3

Finally, a single crate is still moved from stack 1 to stack 2,
but now it's crate C that gets moved:

        [D]
        [N]
        [Z]
[M] [C] [P]
 1   2   3

In this example, the CrateMover 9001 has put the crates in a totally different order: MCD.

Before the rearrangement process finishes, update your simulation so that
the Elves know where they should stand to be ready to unload the final supplies.
After the rearrangement procedure completes, what crate ends up on top of each stack?
 */
object DataDefs:
  type Crate = Char
  type Stack = List[Crate]
  case class Move(count: Int, from: Int, to: Int)

  case class Cargo(stacks: Seq[Stack]):
    def next(move: Move)(reverse: Boolean): Cargo =
      val movedCrates = stacks(move.from).take(move.count)
      val orderedCrates = if reverse then movedCrates.reverse else movedCrates
      val newStacks = stacks.zipWithIndex.map: (stack, index) =>
        if index == move.from then stack.drop(move.count)
        else if index == move.to then orderedCrates ::: stack
        else stack
      Cargo(newStacks)

object Parsing:
  import DataDefs.*

  private def parseStacks(stacks: Seq[String]): Cargo = Cargo(stacks.map(_.toList))

  private def parseMove(move: String): Move = move match
    case s"move $ct from $from to $to" => Move(ct.toInt, from.toInt - 1, to.toInt - 1)

  private def parseMoves(moves: Seq[String]): Seq[Move] = moves map parseMove

  def parse(stacks: Seq[String])(moves: Seq[String]) =
    (parseStacks(stacks), parseMoves(moves))

object Solving:
  import DataDefs.*

  private def solve(stacks: Seq[String])(mvs: Seq[String])(reverse: Boolean) =
    val (cargo, moves) = Parsing.parse(stacks)(mvs)
    val finalCargo = moves.foldLeft(cargo)((c, move) => c.next(move)(reverse))
    finalCargo.stacks.flatMap(_.headOption).mkString

  def solve1(stacks: Seq[String])(mvs: Seq[String]) = solve(stacks)(mvs)(true)
  def solve2(stacks: Seq[String])(mvs: Seq[String]) = solve(stacks)(mvs)(false)

object Testing:
  private lazy val stacks = os.read.lines(os.pwd / "2022" / "05" / "05.test.input.1.txt")
  private lazy val moves = os.read.lines(os.pwd / "2022" / "05" / "05.test.input.2.txt")
  lazy val result1 = Solving.solve1(stacks)(moves)
  lazy val result2 = Solving.solve2(stacks)(moves)
// Testing.result1 // part 1: CMZ
// Testing.result2 // part 2: MCD

object Main:
  private lazy val stacks = os.read.lines(os.pwd / "2022" / "05" / "05.input.1.txt")
  private lazy val moves = os.read.lines(os.pwd / "2022" / "05" / "05.input.2.txt")
  lazy val result1 = Solving.solve1(stacks)(moves)
  lazy val result2 = Solving.solve2(stacks)(moves)
// Main.result1 // part 1: VCTFTJQCG
// Main.result2 // part 2: GCFGLDNJZ
