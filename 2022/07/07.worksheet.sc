object DataDefs:
  enum Command:
    case CdIn(dirName: String)
    case CdOut
    case Ls
    case Dir(name: String)
    case File(name: String, size: Long)
  import Command.*

  case class Directory(
      name: String,
      parent: Option[Directory] = None,
      var dirs: List[Directory] = Nil,
      var files: List[File] = Nil
  ):
    override def toString: String = dirs match
      case Nil => files.toString
      case _   => s"${files.toString}, ${dirs.map(_.toString)}"

    lazy val totalSize: Long          = files.map(_.size).sum + dirs.map(_.totalSize).sum
    lazy val allDirs: List[Directory] = this :: dirs.flatMap(_.allDirs)

object Parsing:
  import DataDefs.*, Command.*

  private def parseLine(line: String): Command = line match
    case s"$$ cd .."       => CdOut
    case s"$$ cd $dirName" => CdIn(dirName)
    case s"$$ ls"          => Ls
    case s"dir $name"      => Dir(name)
    case s"$size $name"    => File(name, size.toLong)

  def parse(lines: Seq[String]): Directory =
    val root                       = Directory("/")
    var current: Option[Directory] = Some(root)
    var parent: Option[Directory]  = Some(root)

    for line <- lines.tail do
      parseLine(line) match
        case CdIn(dirName) =>
          parent = current
          current = current.get.dirs.find(_.name == dirName)
        case CdOut =>
          current = parent
          parent = current.get.parent
        case Ls => // do nothing
        case Dir(name) =>
          current.get.dirs = Directory(name, current) :: current.get.dirs
        case File(name, size) =>
          current.get.files = File(name, size) :: current.get.files
    root

object Solving:
  import DataDefs.*

  def solve1(lines: Seq[String]) = Parsing
    .parse(lines)
    .allDirs
    .filter(_.totalSize <= 100000)
    .map(_.totalSize)
    .sum

  def solve2(lines: Seq[String]) =
    val root   = Parsing.parse(lines)
    val unused = 70000000 - root.totalSize
    root.allDirs
      .filter(dir => unused + dir.totalSize >= 30000000)
      .minBy(_.totalSize)
      .totalSize

object Test:
  val file  = os.pwd / "2022" / "07" / "07.test.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 95437
// Test.res2 // part 2: 24933642

object Main:
  val file  = os.pwd / "2022" / "07" / "07.input.txt"
  val lines = os.read.lines(file)
  val res1  = Solving.solve1(lines)
  val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 1243729
// Main.res2 // part 2: 4443914
