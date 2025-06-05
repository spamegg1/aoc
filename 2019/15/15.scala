package aoc2019.day15

import aoc2019.day09.DataDefs.*
import collection.mutable.{Map => MMap, PriorityQueue => PQueue}

object DataDefs:
  type Pos   = (x: Int, y: Int)
  type Cost  = Int
  type Costs = MMap[Pos, Cost]
  type Path  = Option[(Pos, Cpu)]

  extension (p: Pos) def +(that: Pos): Pos = (x = p.x + that.x, y = p.y + that.y)

  val neighbors: Seq[(Pos, Int)] = Seq(
    (x = 0, y = -1) -> 1,
    (x = 0, y = 1)  -> 2,
    (x = -1, y = 0) -> 3,
    (x = 1, y = 0)  -> 4
  )

object Parsing:
  def parse(line: String) = line.split(",").map(_.toLong).toSeq

object Solving:
  import DataDefs.*, State.Out

  def move(pos: Pos, cpu: Cpu)(delta: Pos, instr: Int): (Pos, Cpu, Long) =
    val nextPos = pos + delta
    val nextCpu = cpu.withIn(instr).nextOut
    val Out(st) = nextCpu.state: @unchecked
    (nextPos, nextCpu, st)

  def dijkstra(initCpu: Cpu): (Costs, Path) =
    val start  = (x = 0, y = 0)
    val costs  = MMap(start -> 0)
    val order  = Ordering.by[(Pos, Cpu), Int]((pos, _) => costs(pos))
    val todo   = PQueue(start -> initCpu)(using order)
    var target = Option.empty[(Pos, Cpu)]

    while todo.nonEmpty do
      val (pos, cpu) = todo.dequeue()
      neighbors
        .map(move(pos, cpu))
        .filter: (nextPos, _, _) =>
          !costs.contains(nextPos) || costs(pos) + 1 < costs(nextPos)
        .filter((_, _, status) => 0 < status)
        .foreach: (nextPos, nextCpu, status) =>
          costs(nextPos) = costs(pos) + 1
          todo.enqueue(nextPos -> nextCpu)
          if status == 2 then target = Some(nextPos -> nextCpu)

    (costs, target)

  def solve1(line: String) =
    val initCpu               = Cpu(Parsing.parse(line))
    val (costs, Some(pos, _)) = dijkstra(initCpu): @unchecked
    costs(pos)

  def solve2(line: String) =
    val initCpu           = Cpu(Parsing.parse(line))
    val (_, Some(_, cpu)) = dijkstra(initCpu): @unchecked
    val (costs, _)        = dijkstra(cpu)
    costs.values.max

object Main:
  lazy val file = os.pwd / "2019" / "15" / "15.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 424
  println(Main.res2) // part 2: 446
