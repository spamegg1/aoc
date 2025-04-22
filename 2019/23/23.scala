package aoc2019.day23

import aoc2019.day09.DataDefs.*

object DataDefs:
  case class Comp(cpu: Cpu, in: Seq[Long], out: Seq[Long]):
    val nextCpu =
      if cpu.in.nonEmpty then cpu.next
      else if in.nonEmpty then cpu.withIn(in*).next
      else cpu.withIn(-1).next
    def nextIn = if cpu.in.nonEmpty then in else Seq()
    def nextOut = nextCpu.state match
      case State.Out(value) => out.appended(value)
      case _                => out
    def next = Comp(nextCpu, nextIn, nextOut)

  type Network = Seq[Comp]
  type NAT     = Seq[Long]

  extension (network: Network)
    def step(nat: NAT, adr: Int): (Network, NAT) =
      val nextNetwork = network.map(_.next)
      nextNetwork.zipWithIndex
        .foldLeft((nextNetwork, nat)):
          case ((network, nat), (comp, src)) =>
            if comp.out.length != 3 then (network, nat)
            else
              val dest    = comp.out.head.toInt
              val nextSrc = comp.copy(out = Seq())
              if dest == adr then (network.updated(src, nextSrc), comp.out.tail)
              else
                val nextIn   = network(dest).in ++ comp.out.tail
                val nextDest = network(dest).copy(in = nextIn)
                (network.updated(src, nextSrc).updated(dest, nextDest), nat)

  object Network:
    def apply(code: Seq[Long]) = Seq.tabulate(50)(i => Comp(Cpu(code), Seq(i), Seq()))

object Parsing:
  def parse(line: String) = line.split(",").toSeq.map(_.toLong)

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def helper1(network: Network, nat: NAT, adr: Int): Long = network.step(nat, adr) match
    case (_, Seq(_, y))         => y
    case (nextNetwork, nextNat) => helper1(nextNetwork, nextNat, adr)

  def solve1(adr: Int)(line: String) = helper1(Network(Parsing.parse(line)), Seq(), adr)

  @annotation.tailrec
  def helper2(nw: Network, nat: NAT, idle: Int, prevIdleY: Option[Long], adr: Int): Long =
    val (nextNw, nextNat, nextIdle, nextIdleY) =
      if idle < 700 then // rough guess
        val (nextNw, nextNat) = nw.step(nat, adr)
        val active            = nw.exists(comp => comp.in.nonEmpty || comp.out.nonEmpty)
        val nextIdle          = if active then 0 else idle + 1
        (nextNw, nextNat, nextIdle, None)
      else
        val nextNw = nw.updated(0, nw(0).copy(in = nat))
        (nextNw, nat, 0, Some(nat.last))

    (prevIdleY, nextIdleY) match
      case (Some(previous), Some(next)) if previous == next => next
      case _ => helper2(nextNw, nextNat, nextIdle, nextIdleY.orElse(prevIdleY), adr)

  def solve2(adr: Int)(line: String) =
    helper2(Network(Parsing.parse(line)), Seq(), 0, None, adr)

object Main:
  lazy val file = os.pwd / "2019" / "23" / "23.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(255)(line)
  lazy val res2 = Solving.solve2(255)(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 27061
  println(Main.res2) // part 2: 19406
