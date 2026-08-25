object DataDefs:
  type Crate = Char
  type Stack = List[Crate]
  case class Move(count: Int, from: Int, to: Int)

  case class Cargo(stacks: Seq[Stack]):
    def next(move: Move)(reverse: Boolean): Cargo =
      val movedCrates   = stacks(move.from).take(move.count)
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
    val finalCargo     = moves.foldLeft(cargo)((c, move) => c.next(move)(reverse))
    finalCargo.stacks.flatMap(_.headOption).mkString

  def solve1(stacks: Seq[String])(mvs: Seq[String]) = solve(stacks)(mvs)(true)
  def solve2(stacks: Seq[String])(mvs: Seq[String]) = solve(stacks)(mvs)(false)

object Test:
  val file1  = os.pwd / "2022" / "05" / "05.test.input.1.txt"
  val file2  = os.pwd / "2022" / "05" / "05.test.input.2.txt"
  val stacks = os.read.lines(file1)
  val moves  = os.read.lines(file2)
  val res1   = Solving.solve1(stacks)(moves)
  val res2   = Solving.solve2(stacks)(moves)
// Test.res1 // part 1: CMZ
// Test.res2 // part 2: MCD

object Main:
  val file1  = os.pwd / "2022" / "05" / "05.input.1.txt"
  val file2  = os.pwd / "2022" / "05" / "05.input.2.txt"
  val stacks = os.read.lines(file1)
  val moves  = os.read.lines(file2)
  val res1   = Solving.solve1(stacks)(moves)
  val res2   = Solving.solve2(stacks)(moves)
// Main.res1 // part 1: VCTFTJQCG
// Main.res2 // part 2: GCFGLDNJZ
