object DataDefs:
  case class Node(
      kidCapacity: Int,
      metaCapacity: Int,
      var kids: List[Node],
      var metadata: List[Int]
  ):
    def metaTotal: Int                     = metadata.sum + kids.map(_.metaTotal).sum
    def addKid(kid: Node): Unit            = kids = kid :: kids
    def reverseKids: Unit                  = kids = kids.reverse
    def addMetadata(meta: List[Int]): Unit = metadata = meta
    def isFull: Boolean                    = kids.size == kidCapacity
    def value: Int =
      if kidCapacity == 0 then metadata.sum
      else
        metadata.view
          .filter(_ <= kidCapacity)
          .map(index => kids(index - 1).value)
          .sum

object Parsing:
  import DataDefs.*
  def parseFile(line: String): List[Int] = line.split(" ").map(_.toInt).toList

object Solving:
  import DataDefs.*

  @annotation.tailrec
  private def node(data: List[Int], stack: List[Node]): List[Node] =
    if data.isEmpty then stack // end
    else if stack.isEmpty || !stack.head.isFull then
      val (kidCap, metaCap, rest) = (data.head, data.tail.head, data.tail.tail)
      val newNode                 = Node(kidCap, metaCap, Nil, Nil)
      node(rest, newNode :: stack)
    else
      val metaCap          = stack.head.metaCapacity
      val (metadata, rest) = (data.take(metaCap), data.drop(metaCap))
      stack.head.addMetadata(metadata)
      stack.head.reverseKids
      stack.tail match
        case neck :: _ =>
          neck.addKid(stack.head)
          node(rest, stack.tail)
        case Nil => node(rest, stack)

  private def findRoot(line: String) = node(Parsing.parseFile(line), Nil).headOption
  def solve1(line: String)           = findRoot(line).map(_.metaTotal)
  def solve2(line: String)           = findRoot(line).map(_.value)

object Test:
  lazy val file = os.pwd / "2018" / "08" / "08.test.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Test.res1 // part 1: 138
// Test.res2 // part 2: 66

object Main:
  lazy val file = os.pwd / "2018" / "08" / "08.input.txt"
  lazy val line = os.read.lines(file).head
  lazy val res1 = Solving.solve1(line)
  lazy val res2 = Solving.solve2(line)
// Main.res1 // part 1: 43825
// Main.res2 // part 2: 19276
