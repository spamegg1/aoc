package aoc2024.day24

import collection.mutable.{Map => MMap, Queue => MQueue}

object DataDefs:
  type ID    = String
  type Wire  = String
  type Wires = MMap[Wire, Int]
  type Gates = Map[ID, Gate] // Gate defined below
  type Conns = Map[Wire, Seq[ID]]

  enum Logic:
    case And, Or, Xor
    def eval: (Int, Int) => Int = this match
      case And => _ & _
      case Or  => _ | _
      case Xor => _ ^ _
  import Logic.*

  extension (s: String)
    def toLogic = s match
      case "AND" => And
      case "OR"  => Or
      case "XOR" => Xor

  case class Gate(id: ID, logic: Logic, l: Wire, r: Wire, out: Wire):
    var lVal    = -1
    var rVal    = -1
    var output  = -1
    def isReady = lVal != -1 && rVal != -1
    def update(wire: Wire, value: Int): Unit =
      if wire == l then lVal = value
      else if wire == r then rVal = value
      else ()
    def eval: Unit = if isReady then output = logic.eval(lVal, rVal) else ()

object Parsing:
  import DataDefs.*, Logic.*

  def parseWire(line: String): (Wire, Int) = line match
    case s"$wire: $value" => (wire, value.toInt)

  def parseGate(line: String): Gate = line match
    case s"$l $logic $r -> $out" => Gate(line, logic.toLogic, l, r, out)

  def parse(lines: String): (Wires, Conns, Gates) =
    val Seq(top, bot) = lines.split("\n\n").toSeq
    val startWires    = MMap.from(top.linesIterator.map(parseWire))
    val gates         = bot.linesIterator.map(parseGate).toSeq
    val allWires      = gates.flatMap(gate => Seq(gate.l, gate.r, gate.out))
    val connsMap = allWires.distinct
      .map: wire => // wire -> Seq(ids of gates wire is attached to)
        wire -> gates.filter(g => g.l == wire || g.r == wire).map(_.id)
      .toMap
    val gatesMap = gates.map(g => g.id -> g).toMap // id -> gate pairs
    (startWires, connsMap, gatesMap)

object Solving:
  import DataDefs.*, Logic.*

  def solve1(lines: String) =
    val (wires, conns, gates) = Parsing.parse(lines)
    val wireQueue             = MQueue.from(wires)

    while wireQueue.nonEmpty do
      val (wire, value) = wireQueue.dequeue()
      val gateIds       = conns(wire)
      for id <- gateIds do
        val gate = gates(id)
        gate.update(wire, value)
        if gate.isReady then
          gate.eval
          wires.getOrElseUpdate(gate.out, gate.output)
          wireQueue.enqueue((gate.out, gate.output))
    end while

    wires
      .filterKeys(_.startsWith("z"))
      .toSeq
      .sortBy(_._1)
      .reverse
      .map(_._2)
      .mkString
  end solve1

  def solve2(lines: String) = 0L

object Test:
  val file  = os.pwd / "2024" / "24" / "24.test.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

object Main:
  val file  = os.pwd / "2024" / "24" / "24.input.txt"
  val lines = os.read(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)

@main
def run: Unit =
  // println(Test.res1) // part 1: 0011111101000
  // println(Test.res2) // part 2:
  // println(Main.res1) // part 1: 1000001100011001000110101100111101110001110000
  // println(Main.res2) // part 2:
  ()
