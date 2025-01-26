package aoc2015.day24

object DataDefs:
  enum Tree:
    case Leaf(set: Set[Int])
    case Half(right: Tree)
    case Fork(left: Tree, right: Tree)

    def allLeaves: List[Set[Int]] = this match
      case Leaf(set)         => List(set)
      case Half(right)       => right.allLeaves
      case Fork(left, right) => left.allLeaves ::: right.allLeaves

object Parsing:
  def parse(lines: Seq[String]) = lines.map(_.toInt).toList.reverse

object Solving:
  import DataDefs.*, Tree.*

  def add(total: Int, head: Int, tree: Tree): Tree = tree match
    case Leaf(set) =>
      if set.sum == total then tree
      else if set.sum + head <= total then Fork(Leaf(set.incl(head)), tree)
      else Half(tree)
    case Half(right)       => Half(add(total, head, right))
    case Fork(left, right) => Fork(add(total, head, left), add(total, head, right))

  @annotation.tailrec
  def makeTree(total: Int, nums: List[Int], acc: Tree): Tree = nums match
    case head :: next => makeTree(total, next, add(total, head, acc))
    case Nil          => acc

  def sets(nums: List[Int]): List[Set[Int]] =
    val total = nums.sum / 3
    val tree  = makeTree(total, nums, Leaf(Set()))
    tree.allLeaves.filter(_.sum == total)

  def partitions(nums: Set[Int], sets: List[Set[Int]]) = sets
    .combinations(3)
    .filter(combo => combo.tail.foldLeft(combo.head)(_ union _) == nums)
    .map(_.sortBy(_.size))
    .toList

  def solve1(lines: Seq[String]) =
    val nums        = Parsing.parse(lines) // descending
    val triples     = partitions(nums.toSet, sets(nums))
    val fewest      = triples.minBy(_.head.size).head.size
    val mostLegroom = triples.filter(_.head.size == fewest)
    mostLegroom.minBy(_.head.product).head.product

  def solve2(lines: Seq[String]) = 0L

object Test:
  lazy val file  = os.pwd / "2015" / "24" / "24.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines) // 99
  lazy val res2  = Solving.solve2(lines)

object Main:
  lazy val file  = os.pwd / "2015" / "24" / "24.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res1  = Solving.solve1(lines)
  lazy val res2  = Solving.solve2(lines)

@main
def run: Unit =
  println(Test.res1)
  println(Test.res2)
  println(Main.res1)
  println(Main.res2)
