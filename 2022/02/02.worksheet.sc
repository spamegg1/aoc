object DataDefs:
  enum Outcome:
    case Loss, Draw, Win
    lazy val score = this.ordinal * 3
  import Outcome.*

  enum Play:
    case Rock, Paper, Scissors
    lazy val score = this.ordinal + 1

    def strategize(outcome: Outcome): Play = (this, outcome) match
      case (Rock, Win) | (Scissors, Loss) | (Paper, Draw) => Paper
      case (Paper, Win) | (Rock, Loss) | (Scissors, Draw) => Scissors
      case (Scissors, Win) | (Paper, Loss) | (Rock, Draw) => Rock
  import Play.*

  extension (s: String)
    def toPlay: Play = s match
      case "A" | "X" => Rock
      case "B" | "Y" => Paper
      case "C" | "Z" => Scissors

    def toOutcome: Outcome = s match
      case "X" => Loss
      case "Y" => Draw
      case "Z" => Win

  case class Round(opponent: Play, me: Play):
    lazy val outcome: Outcome = (opponent, me) match
      case (Rock, Paper) | (Paper, Scissors) | (Scissors, Rock) => Win
      case (Paper, Rock) | (Scissors, Paper) | (Rock, Scissors) => Loss
      case _                                                    => Draw

    lazy val score: Int = me.score + outcome.score

object Parsing:
  import DataDefs.*

  def parseRound1(line: String): Round = line match
    case s"$opponent $me" => Round(opponent.toPlay, me.toPlay)

  def parseRound2(line: String): Round = line match
    case s"$opponent $outcome" =>
      Round(opponent.toPlay, opponent.toPlay.strategize(outcome.toOutcome))

object Solving:
  import DataDefs.Round

  def solve(parser: String => Round)(lines: Seq[String]) = lines.view
    .map(parser)
    .map(_.score)
    .sum

  val solve1 = solve(Parsing.parseRound1)
  val solve2 = solve(Parsing.parseRound2)

object Test:
  val file  = os.pwd / "2022" / "02" / "02.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 15
// Test.res2 // part 2: 12

object Main:
  val file  = os.pwd / "2022" / "02" / "02.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 10816
// Main.res2 // part 2: 11657
