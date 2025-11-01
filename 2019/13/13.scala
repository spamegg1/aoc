package aoc2019.day13

import aoc2019.day09.DataDefs.{Cpu, State}

object DataDefs:
  type Pos    = (x: Long, y: Long)
  type Tiles  = Map[Pos, Long]
  type Score  = Long
  type Paddle = Long
  type Ball   = Long
  type Output = Seq[Long]

  extension (cpu: Cpu)
    def raw(amount: Int): Seq[Cpu] = Iterator
      .iterate(cpu)(_.nextOut)
      .drop(1)
      .take(amount)
      .toSeq
    def take(amount: Int): (Cpu, Output) =
      val raw = cpu.raw(amount)
      val out = raw.map(_.state).collect { case State.Out(value) => value }
      (raw.last, out)

  case class World(score: Score, paddle: Paddle, ball: Ball, tiles: Tiles):
    def next(out: Output): World = out
      .grouped(3)
      .foldLeft(this):
        case (world, Seq(x, y, id)) =>
          val nextScore = if x == -1 then id else world.score
          val nextPad   = if id == 3 then x else world.paddle
          val nextBall  = if id == 4 then x else world.ball
          val nextTiles = world.tiles.updated((x, y), id)
          World(nextScore, nextPad, nextBall, nextTiles)
        case _ => this

  case class Game(cpu: Cpu, amount: Int, world: World):
    def play: Score =
      val (nextCpu, out) = cpu.take(amount)
      val nextWorld      = world.next(out)
      val finalCpu       = nextCpu.withIn((nextWorld.ball - nextWorld.paddle).sign)
      val nextBlocks     = nextWorld.tiles.values.count(_ == 2)
      if nextBlocks == 0 then nextWorld.score
      else Game(finalCpu, 6, nextWorld).play // why 6?

object Parsing:
  def parse(line: String) = line.split(",").map(_.toLong).toSeq

object Solving:
  import DataDefs.*

  def solve1(line: String) =
    val mem = Parsing.parse(line)
    Cpu(mem).allOut
      .grouped(3)
      .foldLeft(Map.empty[Pos, Long]): (tiles, seq) =>
        val Seq(x, y, id) = seq
        tiles.updated((x, y), id)
      .values
      .count(_ == 2)

  def solve2(line: String) =
    val mem   = Parsing.parse(line)
    val cpu   = Cpu(mem.updated(0, 2))
    val world = World(-1, -1, -1, Map())
    val game  = Game(cpu, 2643, world) // 2643 = (20 * 44 + 1) * 3
    game.play

object Main:
  lazy val file = os.pwd / "2019" / "13" / "13.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Main.res1) // part 1: 277
  println(Main.res2) // part 2: 12856
