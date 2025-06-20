package aoc2020.day21

object DataDefs:
  val regex = "(.+) \\(contains (.+)\\)".r
  type Food      = String
  type FoodPair  = (Food, Food)
  type FoodPairs = List[FoodPair]
  type Ingrs     = Set[Food]
  type IngrPairs = (Ingrs, Ingrs)
  type Contents  = Map[Food, Ingrs]

object Parsing:
  import DataDefs.*
  def parseLine(line: String): IngrPairs = line match
    case regex(ingredients, allergens) =>
      (ingredients.split(" ").toSet, allergens.split(", ").toSet)

  def parse(lines: Seq[String]): Seq[IngrPairs] = lines map parseLine

object Solving:
  import DataDefs.*

  def partition(list: Seq[IngrPairs]): (Contents, Contents) =
    val candidates = list.foldLeft(Map.empty[Food, Ingrs]):
      case (candidates, (ingredients, allergens)) =>
        ingredients.foldLeft(candidates): (candidates, ingredient) =>
          candidates.updated(
            ingredient,
            candidates.getOrElse(ingredient, Set()) ++ allergens
          )

    val reduced = candidates.map: (candidate, possible) =>
      val nextReduced = list.foldLeft(possible):
        case (possible, (ingredients, allergens)) =>
          if ingredients.contains(candidate) then possible
          else possible -- allergens
      (candidate, nextReduced)

    reduced.partition((_, possible) => possible.isEmpty)
  end partition

  @annotation.tailrec
  def helper(remain: Contents, known: FoodPairs): FoodPairs =
    if remain.isEmpty then known
    else
      val (ingr, allergen) = remain
        .find((_, allergens) => allergens.size == 1)
        .get
      val nextRemain = remain
        .removed(ingr)
        .view
        .mapValues(_ - allergen.head)
        .toMap
      val nextKnown = (ingr, allergen.head) :: known
      helper(nextRemain, nextKnown)

  def findKnownIngrs(risky: Contents): Food = helper(risky, Nil)
    .sortBy(_._2)
    .map(_._1)
    .mkString(",")

  def solve1(lines: Seq[String]) =
    val list           = Parsing.parse(lines)
    val (inert, risky) = partition(list)
    list
      .map: (ingredients, _) =>
        inert.keySet
          .intersect(ingredients)
          .size
      .sum

  def solve2(lines: Seq[String]) =
    val list       = Parsing.parse(lines)
    val (_, risky) = partition(list)
    findKnownIngrs(risky)

object Test:
  lazy val file  = os.pwd / "2020" / "21" / "21.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2020" / "21" / "21.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1) // part 1: 5
  println(Test.res2) // part 2: mxmxvkd,sqjhc,fvjkl
  println(Main.res1) // part 1: 2614
  println(Main.res2) // part 2: qhvz,kbcpn,fzsl,mjzrj,bmj,mksmf,gptv,kgkrhg
