package aoc2019.day02

object DataDefs:
  type Pos = Int
  enum Op:
    case Add(input1: Pos, input2: Pos, pos: Pos)
    case Multiply(input1: Pos, input2: Pos, pos: Pos)
    case Halt
  import Op.*

object Parsing:
  import DataDefs.*, Op.*

  def parseLine(line: String): Array[Int] = line.split(",").map(_.toInt)

  private def parseOp(numbers: List[Int]): Op = numbers match
    case 1 :: pos1 :: pos2 :: pos3 :: Nil => Add(pos1, pos2, pos3)
    case 2 :: pos1 :: pos2 :: pos3 :: Nil => Multiply(pos1, pos2, pos3)
    case _                                => throw new java.lang.IllegalArgumentException

  @annotation.tailrec
  private def helper(numbers: List[Int])(acc: List[Op]): List[Op] =
    numbers match
      case 99 :: next => acc :+ Halt
      case (1 | 2) :: next =>
        val (top, bot) = (numbers.take(4), numbers.drop(4))
        helper(bot)(acc :+ parseOp(top))
      case _ :: next => throw new java.lang.IllegalArgumentException
      case Nil       => acc

  def parseOps(numbers: List[Int]): List[Op] = helper(numbers)(Nil)

object Solving:
  import DataDefs.*, Op.*
  import util.boundary, boundary.break

  @annotation.tailrec
  private def processOps(numbers: Array[Int])(ops: List[Op]): Unit = ops match
    case Add(input1, input2, pos) :: next =>
      numbers(pos) = numbers(input1) + numbers(input2)
      processOps(numbers)(next)
    case Multiply(input1, input2, pos) :: next =>
      numbers(pos) = numbers(input1) * numbers(input2)
      processOps(numbers)(next)
    case Halt :: _ => ()
    case Nil       => ()

  def solve1(line: String): Int =
    val numbers = Parsing.parseLine(line)
    val ops     = Parsing.parseOps(numbers.toList)
    numbers(1) = 12
    numbers(2) = 2
    processOps(numbers)(ops) // in-place
    numbers(0)

  def solve2(line: String): Int =
    val numbers      = Parsing.parseLine(line)
    val ops          = Parsing.parseOps(numbers.toList)
    var (noun, verb) = (0, 0)

    boundary:
      for
        input1 <- 0 until 100
        input2 <- 0 until 100
      do
        val nums = numbers.clone()
        nums(1) = input1
        nums(2) = input2
        processOps(nums)(ops) // in-place
        if nums(0) == 19690720 then
          noun = input1
          verb = input2
          break()
    noun * 100 + verb

object Test:
  lazy val file = os.pwd / "2019" / "02" / "02.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)

object Main:
  lazy val file = os.pwd / "2019" / "02" / "02.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)

@main
def run: Unit =
  println(Test.res1) // part 1: 3500
  println(Main.res1) // part 1: 3562672
  println(Main.res2) // part 2: 8250
