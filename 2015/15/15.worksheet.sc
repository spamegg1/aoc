object DataDefs:
  case class Ingr(
      name: String,
      capacity: Long,
      durability: Long,
      flavor: Long,
      texture: Long,
      calories: Long
  )

object Parsing:
  import DataDefs.*
  private def parseLine(line: String) = line match
    case s"$nm: capacity $cap, durability $dur, flavor $fl, texture $tx, calories $cal" =>
      Ingr(nm, cap.toLong, dur.toLong, fl.toLong, tx.toLong, cal.toLong)

  def parse(lines: Seq[String]) = lines map parseLine

object Solving:
  import DataDefs.*

  private def getAllMeasures(ingrs: Seq[Ingr]): Seq[Seq[(Ingr, Long)]] =
    for // change this to 2 vars for Test by commenting out w2, w3
      w1 <- 0L to 100L
      w2 <- 0L to 100L
      w3 <- 0L to 100L
      w4 = 100L - w1 - w2 - w3
    // w4 = 100L - w1
    yield ingrs.zip(Seq(w1, w2, w3, w4))
    // yield ingrs.zip(Seq(w1, w4))

  private def score(measures: Seq[(Ingr, Long)]): Long =
    val capacity   = measures.map((ingr, wght) => ingr.capacity * wght).sum
    val durability = measures.map((ingr, wght) => ingr.durability * wght).sum
    val flavor     = measures.map((ingr, wght) => ingr.flavor * wght).sum
    val texture    = measures.map((ingr, wght) => ingr.texture * wght).sum
    capacity.max(0L) * durability.max(0L) * flavor.max(0L) * texture.max(0L)

  private def calories(measures: Seq[(Ingr, Long)]): Long =
    measures.map((ingr, wght) => ingr.calories * wght).sum

  def solve(pred: Seq[(Ingr, Long)] => Boolean)(lines: Seq[String]) =
    val ingrs       = Parsing.parse(lines)
    val allMeasures = getAllMeasures(ingrs).filter(pred)
    val best        = allMeasures.maxBy(score)
    score(best)

  val solve1 = solve(_ => true)
  val solve2 = solve(calories(_) == 500L)

object Test:
  lazy val file  = os.pwd / "2015" / "15" / "15.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Test.res1 // part 1: 62842880
// Test.res2 // part 2: 57600000

object Main:
  lazy val file  = os.pwd / "2015" / "15" / "15.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)
// Main.res1 // part 1: 13882464
// Main.res2 // part 2: 11171160
