/*
--- Day 7: Some Assembly Required ---
This year, Santa brought little Bobby Tables a set of wires and bitwise logic
gates! Unfortunately, little Bobby is a little under the recommended age range,
and he needs help assembling the circuit.

Each wire has an identifier (some lowercase letters) and can carry a 16-bit
signal (a number from 0 to 65535). A signal is provided to each wire by a gate,
another wire, or some specific value. Each wire can only get a signal from one
source, but can provide its signal to multiple destinations. A gate provides no
signal until all of its inputs have a signal.

The included instructions booklet describes how to connect the parts together:
x AND y -> z means to connect wires x and y to an AND gate, and then connect its
output to wire z.

For example:
123 -> x means that the signal 123 is provided to wire x.
x AND y -> z means that bitwise AND of wire x and wire y is provided to wire z.
p LSHIFT 2 -> q means that the value from wire p is left-shifted by 2 and then
  provided to wire q.
NOT e -> f means that the bitwise complement of the value from wire e is
  provided to wire f.

Other possible gates include OR (bitwise OR) and RSHIFT (right-shift). If, for
some reason, you'd like to emulate the circuit instead, almost all programming
languages (for example, C, JavaScript, or Python) provide operators for these
gates.

For example, here is a simple circuit:

123 -> x
456 -> y
x AND y -> d
x OR y -> e
x LSHIFT 2 -> f
y RSHIFT 2 -> g
NOT x -> h
NOT y -> i

After it is run, these are the signals on the wires:

d: 72
e: 507
f: 492
g: 114
h: 65412
i: 65079
x: 123
y: 456

d -> 72,
e -> 507,
f -> 492,
g -> 114,
h -> -124,
i -> -457,
x -> 123,
y -> 456

In little Bobby's kit's instructions booklet (provided as your puzzle input),
what signal is ultimately provided to wire a?

--- Part Two ---
Now, take the signal you got on wire a, override wire b to that signal, and
reset the other wires (including wire a). What new signal is ultimately provided
to wire a?
 */
object DataDefs:
  type Wire = String
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

  def parseGates(lines: Seq[String]): Map[Wire, Gate] =
    lines.map(parseGate).toMap

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
    circuit(`override`) = signal // override
    given gates: Map[Wire, Gate] = Parsing.parseGates(lines)
    processGates
    circuit(wire)

object Testing:
  private lazy val lines = os.read.lines(os.pwd / "07.test.input.txt")
  lazy val result1 = Solving.solve1(lines)("i")
  lazy val result2 = Solving.solve2(lines)("y")(65079)("i")
Testing.result1 // part 1: 65079
Testing.result2 // part 2: 456

object Main:
  private lazy val lines = os.read.lines(os.pwd / "07.input.txt")
  lazy val result1 = Solving.solve1(lines)("a")
  lazy val result2 = Solving.solve2(lines)("b")(16076)("a")
Main.result1 // part 1: 16076
Main.result2 // part 2: 2797
