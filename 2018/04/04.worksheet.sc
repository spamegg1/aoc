object DataDefs:
  case class Date(year: Int, month: Int, day: Int)
  case class Time(hour: Int, minute: Int)
  case class DT(date: Date, time: Time)

  case class Guard(id: Int, sleeps: DT, wakes: DT):
    lazy val range = sleeps.time.minute until wakes.time.minute

  type Guards = Seq[Guard]

object Parsing:
  import DataDefs.*
  def parseGuardLine(line: String): Int = line match
    case s"[$year-$month-$day $hour:$minute] Guard #$id begins shift" => id.toInt

  def parseLine(line: String) = line match
    case s"[$year-$month-$day $hour:$minute] falls asleep" =>
      DT(Date(year.toInt, month.toInt, day.toInt), Time(hour.toInt, minute.toInt))
    case s"[$year-$month-$day $hour:$minute] wakes up" =>
      DT(Date(year.toInt, month.toInt, day.toInt), Time(hour.toInt, minute.toInt))

  def parseSleepWake(guardId: Int)(lines: Seq[String]) =
    val (sleeps, wakes) = (parseLine(lines.head), parseLine(lines.last))
    Guard(guardId, sleeps, wakes)

  def parseBlock(guardLine: String, rest: Seq[String]): Guards =
    val guardId = parseGuardLine(guardLine)
    rest.grouped(2).map(parseSleepWake(guardId)).toSeq

  @annotation.tailrec
  private def parseByBlock(lines: Seq[String])(guards: Guards): Guards =
    if lines.isEmpty then guards
    else
      val guardLine = lines.takeWhile(_.contains("Guard")).head
      val rest1     = lines.dropWhile(_.contains("Guard"))
      val sleepWake = rest1.takeWhile(!_.contains("Guard"))
      val rest2     = rest1.dropWhile(!_.contains("Guard"))
      parseByBlock(rest2)(guards ++ parseBlock(guardLine, sleepWake))

  def parse(lines: Seq[String]): Guards = parseByBlock(lines.sorted)(Nil)

object Solving:
  import DataDefs.*
  def sleepTime(ranges: Seq[Range]): Int = ranges.map(_.size).sum

  def mostFreq(ranges: Seq[Range]) = ranges
    .flatMap(_.toList)
    .groupMapReduce(identity)(_ => 1)(_ + _)
    .maxBy(_._2)

  def solve1(lines: Seq[String]) =
    val byGuardId         = Parsing.parse(lines).groupMap(_.id)(_.range)
    val sleepTimes        = byGuardId.view.mapValues(sleepTime)
    val mostAsleepGuardId = sleepTimes.maxBy(_._2)._1
    val mostFreqMinute    = mostFreq(byGuardId(mostAsleepGuardId))._1
    mostAsleepGuardId * mostFreqMinute

  def solve2(lines: Seq[String]) =
    val byGuardId       = Parsing.parse(lines).groupMap(_.id)(_.range)
    val asleepFreqs     = byGuardId.view.mapValues(mostFreq)
    val mostAsleepGuard = asleepFreqs.maxBy(_._2._2)
    mostAsleepGuard._1 * mostAsleepGuard._2._1

object Test:
  lazy val file  = os.pwd / "2018" / "04" / "04.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 240
// Test.res2 // part 2: 4455

object Main:
  lazy val file  = os.pwd / "2018" / "04" / "04.input.sorted.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 35184
// Main.res2 // part 2: 37886
