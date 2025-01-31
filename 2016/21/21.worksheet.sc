object DataDefs:
  enum Op:
    case SwapPos(pos1: Int, pos2: Int)
    case SwapLet(let1: Char, let2: Char)
    case RotLeft(steps: Int)
    case RotRight(steps: Int)
    case RotBasedLeft(letter: Char)
    case RotBasedRight(letter: Char)
    case Reverse(pos1: Int, pos2: Int)
    case Move(pos1: Int, pos2: Int)

    def reverse = this match
      case SwapPos(pos1, pos2)   => SwapPos(pos2, pos1)
      case SwapLet(let1, let2)   => SwapLet(let2, let1)
      case RotLeft(steps)        => RotRight(steps)
      case RotRight(steps)       => RotLeft(steps)
      case RotBasedLeft(letter)  => RotBasedRight(letter)
      case RotBasedRight(letter) => RotBasedLeft(letter)
      case Reverse(pos1, pos2)   => Reverse(pos2, pos1)
      case Move(pos1, pos2)      => Move(pos2, pos1)

  import Op.*

  extension (s: String)
    def operate(op: Op): String = op match
      case SwapPos(pos1, pos2) =>
        s.zipWithIndex
          .map: (char, index) =>
            if index == pos1 then s(pos2)
            else if index == pos2 then s(pos1)
            else char
          .mkString
      case SwapLet(let1, let2) =>
        val (pos1, pos2) = (s.indexWhere(_ == let1), s.indexWhere(_ == let2))
        s.operate(SwapPos(pos1, pos2))
      case RotLeft(steps) =>
        s.indices
          .map(index => s((index + steps) % s.length))
          .mkString
      case RotRight(steps) => s.operate(RotLeft(s.length - steps))
      case RotBasedLeft(letter) =>
        val index = s.indexWhere(_ == letter)
        val steps = 1 + index + (if index >= 4 then 1 else 0)
        s.operate(RotLeft(steps % s.length))
      case RotBasedRight(letter) =>
        val index = s.indexWhere(_ == letter)
        val steps = 1 + index + (if index >= 4 then 1 else 0)
        s.operate(RotRight(steps % s.length))
      case Reverse(pos1, pos2) => // assume pos1 < pos2
        s.take(pos1) + s.drop(pos1).take(pos2 - pos1 + 1).reverse + s.drop(pos2 + 1)
      case Move(pos1, pos2) =>
        if pos1 < pos2 then
          val (head, tail) = (s.take(pos1), s.drop(pos2 + 1))
          val mid          = s.drop(pos1 + 1).take(pos2 - pos1)
          head + mid + s"${s(pos1)}" + tail
        else // pos2 < pos1
          val (head, tail) = (s.take(pos2), s.drop(pos1 + 1))
          val mid          = s.drop(pos2).take(pos1 - pos2)
          head + s"${s(pos1)}" + mid + tail

object Parsing:
  import DataDefs.*, Op.*

  def parseLine(line: String) = line match
    case s"swap position $pos1 with position $pos2" => SwapPos(pos1.toInt, pos2.toInt)
    case s"swap letter $let1 with letter $let2"     => SwapLet(let1.head, let2.head)
    case s"rotate left $steps steps"                => RotLeft(steps.toInt)
    case s"rotate right $steps steps"               => RotRight(steps.toInt)
    case s"rotate based on position of letter $let" => RotBasedLeft(let.head)
    case s"reverse positions $pos1 through $pos2"   => Reverse(pos1.toInt, pos2.toInt)
    case s"move position $pos1 to position $pos2"   => Move(pos1.toInt, pos2.toInt)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String])(pass: String) = Parsing
    .parse(lines)
    .foldLeft(pass)((scramble, op) => scramble.operate(op))

  def solve2(lines: Seq[String])(pass: String) = Parsing
    .parse(lines)
    .reverse
    .map(_.reverse)
    .foldLeft(pass)((scramble, op) => scramble.operate(op))

object Test:
  lazy val file  = os.pwd / "2016" / "21" / "21.test.input.txt"
  lazy val lines = os.read.lines(file)
  // abcde,ebcda,edcba,abcde,bcdea,bdeac,abdec,ecabd,decab
  lazy val res1 = Solving.solve1(lines)("abcde")
  lazy val res2 = Solving.solve2(lines)("decab")
// Test.res1 // part 1: decab
// Test.res2 // part 2: abcde

object Main:
  lazy val file  = os.pwd / "2016" / "21" / "21.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)("abcdefgh")
  lazy val res2  = Solving.solve2(lines)("fbgdceah")
// Main.res1 // part 1: gcedfahb
// Main.res2 // part 2: hegbdcfa
