package aoc2018.day07

object DataDefs:
  type Step = String
  type Inst = Map[Step, Set[Step]] // k -> v means step k depends on steps v.
  type Reqr = (lo: Step, hi: Step) // lo depends on hi. hi has to be done first.
  type Task = (step: Step, rem: Int)

  extension (s: Step) def timeCost  = s.head - 'A'                     // part 2
  extension (t: Task) def decrement = (step = t.step, rem = t.rem - 1) // part 2

object Parsing:
  import DataDefs.*

  def parseLine(line: String): Reqr = line match
    case s"Step $hi must be finished before step $lo can begin." => (lo, hi)

  def parse(lines: Seq[String]): Inst =
    val reqs       = lines.map(parseLine)
    val allSteps   = reqs.flatMap(r => Seq(r.lo, r.hi)).toSet
    val instructs  = reqs.groupMap(_.lo)(_.hi).map((lo, his) => lo -> his.toSet)
    val midSteps   = instructs.keys.toSet
    val beginSteps = allSteps -- midSteps
    val beginInstr = beginSteps.map(step => step -> Set.empty[Step])
    instructs ++ beginInstr

object Solving:
  import DataDefs.*

  @annotation.tailrec
  def process(remain: Inst, done: Step): Step =
    if remain.isEmpty then done
    else
      val nextStep = remain.filter(_._2.isEmpty).minBy(_._1)._1
      val nextRem = remain
        .removed(nextStep)
        .view
        .mapValues(_ - nextStep)
        .toMap
      process(nextRem, done + nextStep)

  def solve1(lines: Seq[String]) = process(Parsing.parse(lines), "")

  @annotation.tailrec
  def parallel(workers: Int, time: Int)(remain: Inst, tasks: Seq[Task], secs: Int): Int =
    if remain.isEmpty && tasks.isEmpty then secs
    else if tasks.size < workers && remain.values.exists(_.isEmpty) then
      val nextStep = remain.filter(_._2.isEmpty).minBy(_._1)._1
      val task     = (nextStep, time + nextStep.timeCost)
      parallel(workers, time)(remain.removed(nextStep), tasks.appended(task), secs)
    else
      val (done, todo) = tasks.partition(_.rem == 0)
      val nextRemain   = remain.view.mapValues(_ -- done.map(_.step)).toMap
      val nextTasks    = todo.map(_.decrement)
      parallel(workers, time)(nextRemain, nextTasks, secs + 1)

  def solve2(lines: Seq[String])(workers: Int)(time: Int) =
    parallel(workers, time)(Parsing.parse(lines), Seq.empty[Task], 0)

object Test:
  lazy val file  = os.pwd / "2018" / "07" / "07.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(2)(0)

object Main:
  lazy val file  = os.pwd / "2018" / "07" / "07.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)(5)(60)

@main
def run: Unit =
  println(Test.res1) // part 1: CABDFE
  println(Test.res2) // part 2: 15
  println(Main.res1) // part 1: ABLCFNSXZPRHVEGUYKDIMQTWJO
  println(Main.res2) // part 2: 1157
