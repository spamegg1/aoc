import scala.deriving.*, scala.compiletime.*

// MAGIC
case class Date(year: Int, month: Int, day: Int) derives Ordering
case class Yo(x: String, o: Option[Int]) derives Ordering

@main
def hello =
  println(List(Date(2024, 1, 1), Date(2021, 12, 1)).sorted)
  println(List(Yo("hello", None), Yo("a", Some(25)), Yo("a", Some(10))).sorted)

// THE DIRT THAT POWERS THE MAGIC
inline def tuplify[Cur <: Tuple](inline x: Product, n: Int): Tuple =
  inline erasedValue[Cur] match
    case _: EmptyTuple    => Tuple()
    case _: (tpe *: rest) => x.productElement(n) *: tuplify[rest](x, n + 1)

private inline def deriveProduct[T](p: Mirror.ProductOf[T]): Ordering[T] =
  val tupleOrdering = summonInline[Ordering[p.MirroredElemTypes]]

  scala.math.Ordering.by[T, p.MirroredElemTypes](cls =>
    tuplify[p.MirroredElemTypes](cls.asInstanceOf[Product], 0).asInstanceOf
  )(using tupleOrdering)

extension (o: Ordering.type)
  inline def derived[T](using m: Mirror.Of[T]): Ordering[T] =
    inline m match
      case p: Mirror.ProductOf[T] => deriveProduct(p)
