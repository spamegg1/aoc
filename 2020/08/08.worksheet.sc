import collection.mutable.{Map => MMap}
import collection.immutable.{Map => IMap}
import util.boundary, boundary.break

object DataDefs:
  enum Oper:
    case Acc, Jmp, Nop
  import Oper.*

  extension (s: String)
    def toOper: Oper = s match
      case "acc" => Acc
      case "jmp" => Jmp
      case _     => Nop

  case class Instr(oper: Oper, value: Int, seen: Boolean = false):
    def changeOper(newOper: Oper) = Instr(newOper, value, seen)
    def markSeen                  = Instr(oper, value, true)

  case class Program(
      var instrs: IMap[Int, Instr],
      var current: Int,
      var acc: Int,
      var looped: Boolean = false,
      var terminated: Boolean = false
  ):
    def next: Unit =
      instrs.get(current) match
        case None =>
          println(s"Terminated program by going out of bounds: $current")
          terminated = true
        case Some(instr) =>
          if !instr.seen then
            instrs = instrs.updated(current, instr.markSeen)
            instr.oper match
              case Acc => current += 1; acc += instr.value
              case Jmp => current += instr.value
              case Nop => current += 1
          else
            looped = true
            println(s"Same instruction ran a second time: $current, $instr")

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Instr = line match
    case s"$kind +$value" => Instr(kind.toOper, value.toInt)
    case s"$kind -$value" => Instr(kind.toOper, -value.toInt)

  def parse(lines: Seq[String]): Program =
    val instrs = lines.zipWithIndex.map((line, index) => index -> parseLine(line))
    Program(IMap.from(instrs), 0, 0)

object Solving:
  import DataDefs.*, Oper.*

  def solve1(lines: Seq[String]): Int =
    val program = Parsing.parse(lines)
    var acc     = 0
    while !program.looped do program.next
    program.acc

  def solve2(lines: Seq[String]) =
    val program = Parsing.parse(lines)
    var res     = 0

    boundary:
      for index <- 0 until lines.size do
        val newProgram = program.copy()
        val instr      = newProgram.instrs(index)
        instr.oper match
          case Acc =>
          case Jmp =>
            newProgram.instrs = newProgram.instrs.updated(index, instr.changeOper(Nop))
            println(s"Changed instruction at index: $index")
          case Nop =>
            newProgram.instrs = newProgram.instrs.updated(index, instr.changeOper(Jmp))
            println(s"Changed instruction at index: $index")

        while !newProgram.terminated && !newProgram.looped do newProgram.next
        if newProgram.terminated then
          res = newProgram.acc
          break()
    res

object Test:
  lazy val file  = os.pwd / "2020" / "08" / "08.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 5
// Test.res2 // part 2: 8

object Main:
  lazy val file  = os.pwd / "2020" / "08" / "08.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1801
// Main.res2 // part 2: 2060
