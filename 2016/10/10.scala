object DataDefs:
  type Chip = Int

  enum Id:
    case BotId(id: Int)
    case OutId(id: Int)
  import Id.*

  object Id:
    def apply(kind: String, id: String) = kind match
      case "bot"    => BotId(id.toInt)
      case "output" => OutId(id.toInt)

  enum Instr:
    case Compare(id: BotId, lo: Id, hi: Id)
    case Give(id: BotId, chip: Chip)
  import Instr.*

  case class Bot(id: BotId, chips: List[Chip]):
    def canGive: Boolean    = chips.size == 2
    def giveAll: Bot        = copy(chips = Nil)
    def small               = chips.min
    def big                 = chips.max
    def receive(chip: Chip) = copy(chips = chip :: chips)

  case class Output(id: OutId, chips: List[Chip]):
    def receive(chip: Chip) = copy(chips = chip :: chips)

  case class State(
      bots: Map[BotId, Bot],
      outs: Map[OutId, Output],
      instrs: List[Instr],
      log: Map[(Chip, Chip), BotId]
  ):
    def processInstr: State = instrs match
      case Compare(id, lo, hi) :: next =>
        val bot = bots(id)
        if bot.canGive then
          val newLog = log + ((bot.small, bot.big) -> id)
          val bots1  = bots.updated(id, bot.giveAll)
          val (newBots, newOuts) = (lo, hi) match
            case (BotId(bid1), BotId(bid2)) =>
              (
                bots1
                  .updated(BotId(bid1), bots(BotId(bid1)).receive(bot.small))
                  .updated(BotId(bid2), bots(BotId(bid2)).receive(bot.big)),
                outs
              )
            case (BotId(bid), OutId(oid)) =>
              (
                bots1.updated(BotId(bid), bots(BotId(bid)).receive(bot.small)),
                outs.updated(OutId(oid), outs(OutId(oid)).receive(bot.big))
              )
            case (OutId(oid), BotId(bid)) =>
              (
                bots1.updated(BotId(bid), bots(BotId(bid)).receive(bot.big)),
                outs.updated(OutId(oid), outs(OutId(oid)).receive(bot.small))
              )
            case (OutId(oid1), OutId(oid2)) =>
              (
                bots1,
                outs
                  .updated(OutId(oid1), outs(OutId(oid1)).receive(bot.small))
                  .updated(OutId(oid2), outs(OutId(oid2)).receive(bot.big)),
              )
          State(newBots, newOuts, next, newLog)
        else copy(instrs = next.appended(instrs.head))
      case Give(id, chip) :: next =>
        copy(bots = bots.updated(id, bots(id).receive(chip)), instrs = next)
      case Nil => this

  object State: // create empty starting state
    def apply(botCt: Int, outCt: Int, instrs: List[Instr]) = new State(
      (0 until botCt).map(id => BotId(id) -> Bot(BotId(id), Nil)).toMap,
      (0 until outCt).map(id => OutId(id) -> Output(OutId(id), Nil)).toMap,
      instrs,
      Map[(Chip, Chip), BotId]()
    )

object Parsing:
  import DataDefs.*, Instr.*, Id.*

  private def parseLine(line: String): Instr = line match
    case s"value $chip goes to bot $id" => Give(BotId(id.toInt), chip.toInt)
    case s"bot $id gives low to $kind1 $lo and high to $kind2 $hi" =>
      Compare(BotId(id.toInt), Id(kind1, lo), Id(kind2, hi))

  def parse(lines: Seq[String]): List[Instr] = lines.map(parseLine).toList

object Solving:
  import DataDefs.*, Instr.*, Id.*

  def solve1(lines: Seq[String])(botCt: Int, outCt: Int)(small: Chip, big: Chip) =
    var state = State(botCt, outCt, Parsing.parse(lines))
    while state.instrs.nonEmpty do state = state.processInstr
    state.log.get((small, big))

  def solve2(lines: Seq[String])(botCt: Int, outCt: Int) =
    var state = State(botCt, outCt, Parsing.parse(lines))
    while state.instrs.nonEmpty do state = state.processInstr
    state.outs.values.filter(_.id.id <= 2).map(_.chips.head).product

object Test: // there are 3 bots, 3 outputs in test
  lazy val file  = os.pwd / "2016" / "10" / "10.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(3, 3)(2, 5) // 2
  lazy val res2  = Solving.solve2(lines)(3, 3)       // 5*2*3 = 30

object Main: // there are 210 bots, 21 outputs in main
  lazy val file  = os.pwd / "2016" / "10" / "10.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)(210, 21)(17, 61) // 157
  lazy val res2  = Solving.solve2(lines)(210, 21)         // 1085

@main
def run: Unit =
  println(Test.res1) // bot id 2
  println(Test.res2) // 5 * 2 * 3 = 30
  println(Main.res1) // bot id 157
  println(Main.res2) // 1085
  println("Done.")
