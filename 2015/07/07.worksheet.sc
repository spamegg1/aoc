object DataDefs:
  type Wire   = String
  type Signal = Int
  enum Gate:
    case And(in1: Wire, in2: Wire)
    case Or(in1: Wire, in2: Wire)
    case Not(in: Wire)
    case Lshift(in: Wire, shift: Int)
    case Rshift(in: Wire, shift: Int)
    case Direct(in: Wire)
  import Gate.*

object Parsing:
  import DataDefs.*, Gate.*

  private def parseGate(line: String): (Wire, Gate) = line match
    case s"$in1 AND $in2 -> $out"     => out -> And(in1, in2)
    case s"$in1 OR $in2 -> $out"      => out -> Or(in1, in2)
    case s"NOT $in -> $out"           => out -> Not(in)
    case s"$in LSHIFT $shift -> $out" => out -> Lshift(in, shift.toInt)
    case s"$in RSHIFT $shift -> $out" => out -> Rshift(in, shift.toInt)
    case s"$in -> $out"               => out -> Direct(in)

  def parseGates(lines: Seq[String]): Map[Wire, Gate] = lines.map(parseGate).toMap

object Solving:
  import DataDefs.*, Gate.*
  private var circuit = collection.mutable.Map[Wire, Signal]()

  private def lookup(wire: Wire)(using gates: Map[Wire, Gate]): Signal =
    circuit.getOrElseUpdate(wire, wire.toIntOption.getOrElse(evaluate(wire)))

  private def evaluate(wire: Wire)(using gates: Map[Wire, Gate]): Signal =
    gates(wire) match
      case And(in1, in2)     => lookup(in1) & lookup(in2)
      case Or(in1, in2)      => lookup(in1) | lookup(in2)
      case Not(in)           => ~lookup(in) & 0xffff // clamp down to 16-bits
      case Lshift(in, shift) => (lookup(in) << shift) & 0xffff
      case Rshift(in, shift) => lookup(in) >> shift
      case Direct(in)        => lookup(in)

  private def processGates(using gates: Map[Wire, Gate]): Unit =
    gates.keys.foreach(lookup)

  def solve1(lines: Seq[String])(wire: Wire): Signal =
    circuit = collection.mutable.Map[Wire, Signal]() // reset
    given gates: Map[Wire, Gate] = Parsing.parseGates(lines)
    processGates
    circuit(wire)

  def solve2(lines: Seq[String])(`override`: Wire)(signal: Signal)(wire: Wire) =
    circuit = collection.mutable.Map[Wire, Signal]() // reset
    circuit(`override`) = signal                     // override
    given gates: Map[Wire, Gate] = Parsing.parseGates(lines)
    processGates
    circuit(wire)

object Test:
  lazy val file  = os.pwd / "2015" / "07" / "07.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)("i")
  lazy val res2  = Solving.solve2(lines)("y")(65079)("i")
// Test.res1 // part 1: 65079
// Test.res2 // part 2: 456

object Main:
  lazy val file  = os.pwd / "2015" / "07" / "07.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)("a")
  lazy val res2  = Solving.solve2(lines)("b")(16076)("a")
// Main.res1 // part 1: 16076
// Main.res2 // part 2: 2797
