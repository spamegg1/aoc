package aoc2019.day25

import aoc2019.day09.DataDefs.*

object Parsing:
  def parse(line: String) = line.split(",").toSeq.map(_.toLong)

object Solving:
  def desc(cpu: Cpu): Cpu =
    import State.*

    @annotation.tailrec
    def helper(cpu: Cpu, msg: String): Cpu =
      if msg.endsWith("Command?\n") ||
        msg.endsWith("!\n\n") ||
        msg.endsWith("\"\n")
      then
        print(msg)
        cpu
      else
        val nextCpu = cpu.next
        val nextMsg = nextCpu.state match
          case Out(value) => msg.appended(value.toChar)
          case _          => msg
        helper(nextCpu, nextMsg)
    helper(cpu, "")

  def solve(line: String): Unit =
    @annotation.tailrec
    def helper(cpu: Cpu, command: String, precanned: Seq[String]): Unit =
      if command == "exit" then ()
      else
        val input   = command.map(_.toLong).appended(10L)
        val nextCpu = desc(cpu.withIn(input*))
        val (nextCommand, nextPrecanned) = precanned match
          case first +: tail => (first, tail)
          case _             => (Console.in.readLine(), Seq())
        helper(nextCpu, nextCommand, nextPrecanned)
    val code = Parsing.parse(line)
    helper(Cpu(code), "", Seq())

object Main:
  lazy val file = os.pwd / "2019" / "25" / "25.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res  = Solving.solve(line)

@main
def run: Unit =
  // play the game manually! Pretty annoying...
  println(Main.res) // 100667393
