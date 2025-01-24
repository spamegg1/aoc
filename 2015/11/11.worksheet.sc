object DataDefs:
  val alphabet = "abcdefghijklmnopqrstuvwxyz"
  val excluded = "ilo"
  val pairs    = alphabet.map(_.toString * 2).filterNot(_.isExcluded)
  val threes   = alphabet.sliding(3).filterNot(isExcluded).toSeq

  extension (s: String)
    def isExcluded = s.exists(excluded.contains(_))
    def increment: String =
      if s.isEmpty then s
      else if s.endsWith("z") then s.init.increment + "a"
      else s.init.appended((s.last + 1).toChar)
    def req1    = threes.exists(s.contains(_))
    def req2    = !s.isExcluded
    def req3    = s.sliding(2).toSeq.intersect(pairs).size >= 2
    def isValid = s.req1 && s.req2 && s.req3

object Solving:
  import DataDefs.*

  def solve(pass: String) =
    var nextPass = pass
    while !nextPass.isValid do nextPass = nextPass.increment
    nextPass

object Test:
  lazy val file  = os.pwd / "2015" / "11" / "11.test.input.txt"
  lazy val lines = os.read.lines(file)
  lazy val res   = lines map Solving.solve
// Test.res // abcdffaa, ghjaabcc

object Main:
  import DataDefs.*
  lazy val line = "cqjxjnds"
  lazy val res1 = Solving.solve(line)
  lazy val res2 = Solving.solve(res1.increment)
// Main.res1 // part 1: cqjxxyzz
// Main.res2 // part 2: cqkaabcc
