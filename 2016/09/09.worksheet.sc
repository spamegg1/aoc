object Parsing:
  @annotation.tailrec
  def parse1(line: String)(acc: Int): Int =
    if line.isEmpty then acc
    else
      line match
        case s"$pre(${letters}x$copies)$post" =>
          val (decompress, rest) = (post.take(letters.toInt), post.drop(letters.toInt))
          parse1(rest)(acc + pre.length + decompress.length * copies.toInt)
        case _ => acc + line.length

  def parse2(line: String)(acc: Long): Long =
    if line.isEmpty then acc
    else
      line match
        case s"$pre(${letters}x$copies)$post" =>
          val (decompress, rest) = (post.take(letters.toInt), post.drop(letters.toInt))
          val decompressed       = parse2(decompress)(0L) // deep recursion here
          parse2(rest)(acc + pre.length + decompressed * copies.toLong)
        case _ => acc + line.length

object Solving:
  def solve1(line: String) = Parsing.parse1(line)(0)
  def solve2(line: String) = Parsing.parse2(line)(0L)

object Test:
  lazy val file  = os.pwd / "2016" / "09" / "09.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = lines map Solving.solve1
  lazy val res2  = lines map Solving.solve2
// Test.res1 // part 1: 6,7,9,11,6,18,324,238
// Test.res2 // part 2: 6,7,9,11,3,20,241920,445

object Main:
  lazy val file = os.pwd / "2016" / "09" / "09.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 110346
// Main.res2 // part 2: 10774309173
