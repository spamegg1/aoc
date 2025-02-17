object DataDefs:
  enum State:
    case Running, Halted
  import State.*

  case class Cpu(
      instrs: Seq[String],
      regs: Map[String, Long] = Map(),
      ptr: Int = 0,
      state: State = Running,
      count: Int = 0
  ):
    def next: Cpu               = copy(ptr = ptr + 1)
    def read(key: String): Long = key.toLongOption.getOrElse(regs.getOrElse(key, 0L))
    def write(key: String, value: Long): Cpu = next.copy(regs = regs.updated(key, value))
    def step: Cpu =
      if !instrs.indices.contains(ptr) then copy(state = Halted)
      else
        val Array(op, dest, src) = instrs(ptr).split(" ")
        op match
          case "set" => write(dest, read(src))
          case "sub" => write(dest, read(dest) - read(src))
          case "mul" => write(dest, read(dest) * read(src)).copy(count = count + 1)
          case "jnz" =>
            if read(dest) != 0 then copy(ptr = ptr + read(src).toInt) else next

object Solving:
  import DataDefs.*, State.*

  def solve1(lines: Seq[String]) = Iterator
    .iterate(Cpu(instrs = lines))(_.step)
    .dropWhile(_.state == Running)
    .next()
    .count

  // reverse engineering shows it's counting composite numbers, skipping by 17,
  // between b * 100 + 100000 and b * 100 + 117000, for me b = 65
  def solve2(lines: Seq[String]) =
    val composite = (n: Int) => (2 to math.sqrt(n).toInt).exists(n % _ == 0)
    val b         = 65
    val start     = b * 100 + 100000
    val end       = start + 17000
    (start to end by 17).count(composite)

object Test:
  lazy val file  = os.pwd / "2017" / "23" / "23.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 0
// Test.res2 // part 2: 917

object Main:
  lazy val file  = os.pwd / "2017" / "23" / "23.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 3969
// Main.res2 // part 2: 917
