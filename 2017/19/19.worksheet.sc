object DataDefs:
  enum Dir:
    case N, S, E, W
    def isHorizontal = this match
      case N => false
      case S => false
      case E => true
      case W => true
    def isVertical = this match
      case N => true
      case S => true
      case E => false
      case W => false
  import Dir.*

  object Dir:
    def horizontal = Seq(W, E)
    def vertical   = Seq(N, S)

  type Pos = (x: Int, y: Int)
  extension (p: Pos)
    def move(dir: Dir) = dir match
      case N => (p.x, p.y - 1)
      case S => (p.x, p.y + 1)
      case E => (p.x + 1, p.y)
      case W => (p.x - 1, p.y)
    def neighbors = Dir.values.map(p.move)

  type Path  = Seq[Char]
  type Steps = Int
  type Grid  = Seq[String]

  extension (g: Grid)
    def charAt(pos: Pos): Char = g(pos.y)(pos.x)
    def walk: (Path, Steps) =
      @annotation.tailrec
      def helper(pos: Pos, dir: Dir, path: Path, steps: Int): (Path, Steps) =
        val nextPos = pos.move(dir)
        g.charAt(nextPos) match
          case ' ' => (path, steps)
          case '+' =>
            val Seq(d1, d2) = if dir.isHorizontal then Dir.vertical else Dir.horizontal
            val nextDir     = if g.charAt(nextPos.move(d1)) != ' ' then d1 else d2
            helper(nextPos, nextDir, path, steps + 1)
          case c if c.isLetter => helper(nextPos, dir, path.appended(c), steps + 1)
          case _               => helper(nextPos, dir, path, steps + 1)

      helper((g.head.indexOf('|'), -1), S, Seq(), 0)

object Solving:
  import DataDefs.*
  def solve1(lines: Seq[String]) = lines.walk._1.mkString
  def solve2(lines: Seq[String]) = lines.walk._2

object Test:
  lazy val file  = os.pwd / "2017" / "19" / "19.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: ABCDEF
// Test.res2 // part 2: 38

object Main:
  lazy val file  = os.pwd / "2017" / "19" / "19.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: PBAZYFMHT
// Main.res2 // part 2: 16072
