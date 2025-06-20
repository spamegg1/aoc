package aoc2020.day22

object DataDefs:
  type Deck = Seq[Int]

  enum Winner(deck: Deck):
    case P1(deck: Deck) extends Winner(deck)
    case P2(deck: Deck) extends Winner(deck)
    def score: Int = deck
      .foldRight((0, 1)):
        case (card, (total, multiplier)) =>
          (total + multiplier * card, multiplier + 1)
      ._1
  import Winner.*

  case class Round(p1: Deck, p2: Deck):
    def p1Wins = Round(p1.tail ++ Seq(p1.head, p2.head), p2.tail)
    def p2Wins = Round(p1.tail, p2.tail ++ Seq(p2.head, p1.head))
    def simpleCombat: Winner = this match
      case Round(p1, Seq()) => P1(p1)
      case Round(Seq(), p2) => P2(p2)
      case Round(p1, p2) =>
        val nextRound = if p1.head > p2.head then p1Wins else p2Wins
        nextRound.simpleCombat

object Parsing:
  import DataDefs.*
  def parse(input: Seq[String]): Round =
    val index = input.indexOf("")
    val p1    = input.take(index).drop(1).map(_.toInt)
    val p2    = input.drop(index + 2).map(_.toInt)
    Round(p1, p2)

object Solving:
  import DataDefs.*, Winner.*

  def recursiveCombat(round: Round, prev: Seq[Round] = Seq()): Winner = round match
    case Round(p1, Seq()) => P1(p1)
    case Round(Seq(), p2) => P2(p2)
    case Round(p1, p2) =>
      val nextRound =
        if prev.contains(round) then Round(p1, Seq())
        else if p1.head > p1.tail.size || p2.head > p2.tail.size then
          if p1.head > p2.head then round.p1Wins else round.p2Wins
        else
          val subGame = Round(p1.tail.take(p1.head), p2.tail.take(p2.head))
          recursiveCombat(subGame) match
            case P1(_) => round.p1Wins
            case P2(_) => round.p2Wins
      recursiveCombat(nextRound, prev.appended(round))

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .simpleCombat
    .score

  def solve2(lines: Seq[String]) = recursiveCombat(Parsing.parse(lines)).score

object Test:
  lazy val file  = os.pwd / "2020" / "22" / "22.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "22" / "22.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 306
  println(Test.res2) // part 2: 291
  println(Main.res1) // part 1: 33561
  println(Main.res2) // part 2: 34594 takes about 20 sec
